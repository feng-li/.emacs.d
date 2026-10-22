"""Attachment export checks; real Notmuch tests use an isolated temporary index."""

from contextlib import redirect_stdout
from email.message import EmailMessage
import hashlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

SCRIPT = Path(__file__).parents[1] / 'notmuch-attachments.py'
SPEC = importlib.util.spec_from_file_location('notmuch_attachments', SCRIPT)
exporter = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(exporter)


def message(message_id='one@example.test', parts=None):
    return {'id': message_id, 'headers': {'From': 'Sender', 'Subject': 'Test', 'Date': 'Today'},
            'body': parts or []}


def attachment(part_id=2, filename='report.bin'):
    return {'id': part_id, 'filename': filename, 'content-type': 'application/octet-stream',
            'content-disposition': 'attachment'}


class ExportTests(unittest.TestCase):
    def test_filename_safety_and_unicode_length(self):
        for name, expected in [('../../report.pdf', 'report.pdf'),
                               ('C:\\folder\\report.pdf', 'report.pdf'),
                               ('a\x00b\nc.pdf', 'a_b_c.pdf'),
                               ('..', 'attachment-2.bin'), (' .hidden. ', 'hidden')]:
            self.assertEqual(exporter.safe_filename(name, 'attachment-2.bin'), expected)
        name = exporter.safe_filename('中' * 200 + '.pdf', 'fallback')
        self.assertLessEqual(len(name.encode('utf-8')), 200)
        self.assertTrue(name.endswith('.pdf'))

    def test_exclusive_creation_preserves_files_and_symlinks(self):
        with tempfile.TemporaryDirectory() as temp:
            directory = Path(temp)
            (directory / 'report.pdf').write_bytes(b'original')
            (directory / 'report (2).pdf').symlink_to(directory / 'missing')
            path, output = exporter.create_unique(directory, 'report.pdf')
            with output:
                output.write(b'new')
            self.assertEqual(path.name, 'report (3).pdf')
            self.assertEqual((directory / 'report.pdf').read_bytes(), b'original')
            self.assertTrue((directory / 'report (2).pdf').is_symlink())
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)

    def test_message_walk_includes_unmatched_replies_and_missing_parents(self):
        first, second = message('first'), message('second')
        second['match'] = False
        forest = [[[None, [[first, [[second, []]]], [first, []]]]]]
        self.assertEqual(list(exporter.messages_in(forest)), [first, second])

    def test_mime_walk_omits_body_and_does_not_double_export_attached_message(self):
        enclosed = attachment(4, 'forwarded.eml')
        enclosed['content'] = [attachment(5, 'inside.pdf')]
        inline = {'id': 3, 'filename': 'logo.png', 'content-disposition': 'inline'}
        parts = [{'id': 1, 'content-type': 'multipart/mixed',
                  'content': [{'id': 2, 'content-type': 'text/plain', 'content': 'body'},
                              inline, enclosed]}]
        self.assertEqual(list(exporter.attachments_in(parts)), [inline, enclosed])

    def test_unnamed_attached_file_and_embedded_body_are_found(self):
        part = attachment(filename=None)
        root = {'content': [{'headers': {}, 'body': [part]}]}
        self.assertEqual(list(exporter.attachments_in([root])), [part])

    def test_query_escaping_is_not_shell_interpolation(self):
        self.assertEqual(exporter.id_query('a" or from:*'), 'id:"a"" or from:*"')

    def run_export(self, directory, parts, raw_runner, headers=None):
        msg = message(parts=parts)
        if headers:
            msg['headers'] = headers
        with patch.object(exporter, 'read_json', side_effect=[['abc123'], [[[msg, []]]]]), \
                patch.object(exporter.subprocess, 'run', side_effect=raw_runner), \
                redirect_stdout(io.StringIO()):
            result = exporter.extract({'directory': str(directory), 'message_query': 'id:one'},
                                      progress=False)
        return result

    def test_failed_part_is_removed_and_other_parts_continue(self):
        def runner(args, stdout, **kwargs):
            self.assertIn('--decrypt=auto', args)
            stdout.write(b'partial' if '--part=2' in args else b'complete')
            if '--part=2' in args:
                raise subprocess.CalledProcessError(1, args, stderr=b'cannot read part')
            return subprocess.CompletedProcess(args, 0)

        with tempfile.TemporaryDirectory() as temp:
            result = self.run_export(Path(temp), [attachment(2, 'bad.bin'), attachment(3, 'good.bin')], runner)
            self.assertFalse(result['ok'])
            self.assertEqual((result['saved'], result['errors']), (1, 1))
            self.assertFalse((Path(temp) / 'bad.bin').exists())
            self.assertEqual((Path(temp) / 'good.bin').read_bytes(), b'complete')
            rows = [json.loads(line) for line in Path(result['manifest']).read_text().splitlines()]
            self.assertEqual([row['status'] for row in rows], ['error', 'saved'])
            self.assertIn('cannot read part', rows[0]['error'])

    def test_invalid_part_is_reported_without_running_notmuch(self):
        with tempfile.TemporaryDirectory() as temp:
            def unexpected(*args, **kwargs):
                self.fail('Invalid part should not run notmuch')

            result = self.run_export(Path(temp), [attachment(part_id=None)], unexpected)
            self.assertEqual(result['errors'], 1)
            self.assertEqual(result['saved'], 0)

    def test_no_attachments_creates_no_export_files(self):
        with tempfile.TemporaryDirectory() as temp:
            target = Path(temp) / 'not-created'
            result = self.run_export(target, [], lambda *a, **k: self.fail('Unexpected raw read'))
            self.assertTrue(result['ok'])
            self.assertIsNone(result['manifest'])
            self.assertFalse(target.exists())

    def test_nonunique_or_invalid_thread_fails_before_writing(self):
        for threads in ([], ['a', 'b'], ['not a hex ID']):
            with tempfile.TemporaryDirectory() as temp, \
                    patch.object(exporter, 'read_json', return_value=threads):
                with self.assertRaises(ValueError):
                    exporter.extract({'directory': temp, 'message_query': 'id:one'})
                self.assertEqual(list(Path(temp).iterdir()), [])

    def test_headers_are_indexed_as_data_and_unnamed_files_get_names(self):
        def runner(args, stdout, **kwargs):
            stdout.write(b'\x00\xff\r\n')
            return subprocess.CompletedProcess(args, 0)

        with tempfile.TemporaryDirectory() as temp:
            result = self.run_export(Path(temp), [attachment(filename=None)], runner,
                                     {'From': 'A\nB', 'Subject': '=FORMULA("x")', 'Date': 'Today'})
            rows = [json.loads(line) for line in Path(result['manifest']).read_text().splitlines()]
            self.assertEqual(rows[0]['from'], 'A\nB')
            self.assertEqual(rows[0]['subject'], '=FORMULA("x")')
            self.assertEqual((Path(temp) / rows[0]['saved_filename']).read_bytes(), b'\x00\xff\r\n')


@unittest.skipUnless(shutil.which('notmuch'), 'notmuch CLI is not installed')
class RealNotmuchTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='notmuch-attachment-test-')
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.mail = self.directory / 'mail'
        for subdir in ('new', 'cur', 'tmp'):
            (self.mail / subdir).mkdir(parents=True)
        self.config = self.directory / 'notmuch-config'
        self.config.write_text(f'[database]\npath={self.mail}\n[user]\nname=Test\n'
                               'primary_email=test@example.test\n[new]\ntags=unread;inbox\n'
                               '[search]\nexclude_tags=spam;deleted\n[maildir]\nsynchronize_flags=false\n')
        self.env = {key: value for key, value in os.environ.items() if not key.startswith('NOTMUCH_')}
        self.env['NOTMUCH_CONFIG'] = str(self.config)

    def notmuch(self, *args):
        return subprocess.run([shutil.which('notmuch'), *args], env=self.env,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True).stdout

    def add_message(self, name, files=(), parent=None):
        msg = EmailMessage()
        msg['From'] = 'Sender <sender@example.test>'
        msg['To'] = 'Recipient <recipient@example.test>'
        msg['Subject'] = 'Re: Export test' if parent else 'Export test'
        msg['Date'] = 'Thu, 22 Oct 2026 12:00:00 +0800'
        msg['Message-ID'] = f'<{name}@example.test>'
        if parent:
            msg['In-Reply-To'] = f'<{parent}@example.test>'
            msg['References'] = f'<{parent}@example.test>'
        msg.set_content('This body must not be exported as an attachment.')
        for filename, content, disposition in files:
            msg.add_attachment(content, maintype='application', subtype='octet-stream',
                               filename=filename, disposition=disposition)
        path = self.mail / 'new' / name
        path.write_bytes(msg.as_bytes())
        return path

    def run_worker(self, name, target):
        job = {'message_query': exporter.id_query(name + '@example.test'),
               'directory': str(target), 'notmuch_command': shutil.which('notmuch')}
        process = subprocess.run([sys.executable, str(SCRIPT)], input=json.dumps(job), text=True,
                                 env=self.env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        lines = [line for line in process.stdout.splitlines() if line.startswith('NOTMUCH-ATTACHMENTS-RESULT ')]
        self.assertEqual(process.returncode, 0, process.stdout + process.stderr)
        self.assertEqual(len(lines), 1, process.stdout)
        return json.loads(lines[0].split(' ', 1)[1])

    def test_whole_thread_binary_content_collisions_and_read_only_index(self):
        first = self.add_message('root', [('report.bin', b'\x00\xffROOT\r\n', 'attachment'),
                                          ('../../escaped.bin', b'ESCAPED', 'attachment')])
        second = self.add_message('reply', [('report.bin', b'REPLY\x00', 'attachment'),
                                            ('logo.bin', b'LOGO', 'inline')], parent='root')
        unrelated = self.add_message('unrelated', [('secret.bin', b'NOT THIS THREAD', 'attachment')])
        self.notmuch('new')
        # A thread export must include even an excluded/collapsed reply.
        self.notmuch('tag', '+spam', '--', exporter.id_query('reply@example.test'))
        before_tags = self.notmuch('dump', '--output=tags')
        hashes = {path: hashlib.sha256(path.read_bytes()).hexdigest() for path in (first, second, unrelated)}
        output = self.directory / 'exports'
        output.mkdir()
        (output / 'report.bin').write_bytes(b'ORIGINAL')
        secret = self.directory / 'secret'
        secret.write_bytes(b'UNCHANGED')
        (output / 'escaped.bin').symlink_to(secret)
        (output / 'attachments.jsonl').write_text('old manifest')
        result = self.run_worker('root', output)
        self.assertEqual((result['messages'], result['saved'], result['errors']), (2, 4, 0))
        self.assertEqual(Path(result['manifest']).name, 'attachments (2).jsonl')
        self.assertEqual((output / 'report.bin').read_bytes(), b'ORIGINAL')
        self.assertEqual((output / 'report (2).bin').read_bytes(), b'\x00\xffROOT\r\n')
        self.assertEqual((output / 'report (3).bin').read_bytes(), b'REPLY\x00')
        self.assertEqual((output / 'escaped (2).bin').read_bytes(), b'ESCAPED')
        self.assertEqual((output / 'logo.bin').read_bytes(), b'LOGO')
        self.assertEqual(secret.read_bytes(), b'UNCHANGED')
        self.assertFalse((self.directory / 'escaped.bin').exists())
        self.assertFalse((output / 'secret.bin').exists())
        self.assertEqual((output / 'attachments.jsonl').read_text(), 'old manifest')
        rows = [json.loads(line) for line in Path(result['manifest']).read_text().splitlines()]
        self.assertEqual({row['message_id'] for row in rows}, {'root@example.test', 'reply@example.test'})
        self.assertTrue(all(row['status'] == 'saved' for row in rows))
        self.assertEqual(self.notmuch('dump', '--output=tags'), before_tags)
        for path, digest in hashes.items():
            self.assertEqual(hashlib.sha256(path.read_bytes()).hexdigest(), digest)

    def test_thread_without_attachments_does_not_create_destination(self):
        self.add_message('plain')
        self.notmuch('new')
        target = self.directory / 'no-export-needed'
        result = self.run_worker('plain', target)
        self.assertEqual(result['saved'], 0)
        self.assertIsNone(result['manifest'])
        self.assertFalse(target.exists())


if __name__ == '__main__':
    unittest.main()
