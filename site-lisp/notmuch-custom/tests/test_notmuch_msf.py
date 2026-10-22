"""Regression tests for notmuch-custom's summary parsing and validation."""
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("notmuch_msf", Path(__file__).parents[1] / "notmuch-msf.py")
msf = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(msf)

HEADER = '''// <!-- <mdb:mork:z v="1.4"/> -->
< <(a=c)> (80=ns:msg:db:row:scope:msgs:all)(83=message-id)(88=flags)
(96=ns:msg:db:table:kind:msgs)(9F=ns:msg:db:row:scope:dbfolderinfo:all)
(A0=ns:msg:db:table:kind:dbfolderinfo)(AC=version)(AD=forceReparse)>
{1:^9F {(k^A0:c)} [1(^AC=1)(^AD=0)]}
'''


def flags(body):
    return msf.Mork((HEADER + body).encode()).parse().flags()


class ParserTests(unittest.TestCase):
    def test_atoms_and_escaped_literal(self):
        self.assertEqual(flags(r'''<(90=atom@example.org)(91=4)>
{1:^80 {(k^96:c)} [1(^83^90)(^88^91)]
[2(^83=literal$40ex\
ample.org)(^88=1)]}'''), {"atom@example.org": True, "literal@example.org": False})

    def test_appended_updates_and_atom_redefinition(self):
        self.assertEqual(flags('''<(90=old@example.org)>
{1:^80 {(k^96:c)} [1(^83^90)(^88=4)]}
@$${A{@
<(90=new@example.org)>
[1:^80(^88=0)]
{1:^80 [2(^83^90)(^88=4)]}
@$$}A}@'''), {"old@example.org": False, "new@example.org": True})

    def test_row_replacement_and_table_removal(self):
        self.assertEqual(flags('''{1:^80 {(k^96:c)}
[1(^83=old@example.org)(^88=4)] [2(^83=gone@example.org)(^88=4)]}
@$${1{@
[-1:^80(^83=new@example.org)(^88=0)]
{1:^80 -2}
@$$}1}@'''), {"new@example.org": False})

    def test_clear_table_keeps_only_new_members(self):
        self.assertEqual(flags('''{1:^80 {(k^96:c)} [1(^83=old@example.org)(^88=4)]}
{-1:^80 {(k^96:c)} [2(^83=new@example.org)(^88=0)]}'''), {"new@example.org": False})

    def test_cut_cell_does_not_keep_stale_flags(self):
        with self.assertRaises(msf.InvalidMSF):
            flags('''{1:^80 {(k^96:c)} [1(^83=a@example.org)(^88=4)]}
[1:^80-(^88)]''')

    def test_deleted_and_detached_rows_are_unknown(self):
        self.assertEqual(flags('''{1:^80 {(k^96:c)}
[1(^83=deleted@example.org)(^88=200004)]
[2(^83=expunged@example.org)(^88=c)]}
[3:^80(^83=detached@example.org)(^88=4)]'''), {})

    def test_duplicate_message_id_star_wins(self):
        self.assertEqual(flags('''{1:^80 {(k^96:c)}
[1(^83=same@example.org)(^88=4)] [2(^83=same@example.org)(^88=0)]}'''), {"same@example.org": True})

    def test_partial_aborted_mismatched_transactions(self):
        for tail in ('@$${1{@', '@$${1{@ @$$}2}@', '@$${1{@ @$$}~~}@'):
            with self.subTest(tail=tail), self.assertRaises(msf.InvalidMSF):
                flags('{1:^80 {(k^96:c)}}\n' + tail)

    def test_bad_data_rejected(self):
        for body in ('{1:^80 {(k^96:c)} [1(^83^DEAD)(^88=4)]}',
                     '{1:^80 {(k^96:c)} [1(^83=x@example.org)(^88=badflag)]}',
                     '{1:^80 {(k^96:c)} [1(^83=x@example.org)(^88=4)]',
                     '[1:^9F(^AD=1)]', '[1:^9F(^88=20)]'):
            with self.subTest(body=body), self.assertRaises(msf.InvalidMSF):
                flags(body)

    def test_escaped_parenthesis_and_backslash(self):
        self.assertEqual(flags(r'{1:^80 {(k^96:c)} [1(^83=a\)b\\c@example.org)(^88=4)]}'),
                         {"a)b\\c@example.org": True})

    def test_missing_or_changing_file_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / 'Inbox.msf'
            with self.assertRaises(OSError):
                msf.scan([p], progress=False)
            p.write_text(HEADER + '{1:^80 {(k^96:c)}}')
            with patch.object(msf, 'fingerprint', side_effect=[(1,), (2,)]):
                with self.assertRaises(msf.InvalidMSF):
                    msf.scan([p], progress=False)
            with patch.object(msf, 'fingerprint', side_effect=[(1,), (1,), (2,)]):
                with self.assertRaises(msf.InvalidMSF):
                    msf.scan([p], progress=False)

    def test_multi_folder_union_and_empty_not_unstarred(self):
        with tempfile.TemporaryDirectory() as tmp:
            paths = [Path(tmp) / f'{i}.msf' for i in range(3)]
            for p, state in zip(paths, ['4', '0', None]):
                row = f'[1(^83=same@example.org)(^88={state})]' if state else ''
                p.write_text(HEADER + '{1:^80 {(k^96:c)} ' + row + '}')
            self.assertEqual(msf.scan(paths, progress=False),
                             {'starred': ['same@example.org'], 'unstarred': [], 'folders': 3})


if __name__ == '__main__':
    unittest.main()
