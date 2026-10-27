#!/usr/bin/env python3
"""Record Wi-Fi, Tailscale and Mosh diagnostics without changing network settings."""
import argparse
import concurrent.futures
import datetime
import json
import os
from pathlib import Path
import re
import shutil
import math
import shlex
import subprocess
import sys
import tempfile
import threading
import time


def now():
    return datetime.datetime.now().astimezone().isoformat(timespec="milliseconds")


def run(argv, timeout=8):
    if not shutil.which(argv[0]):
        return {"code": None, "output": f"{argv[0]} is not installed"}
    try:
        p = subprocess.run(argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           text=True, timeout=timeout, env={**os.environ, "LC_ALL": "C"})
        output = re.sub(r"(https?://)[^/\s@]+@", r"\1[credentials removed]@", p.stdout)
        return {"code": p.returncode, "output": output.strip()}
    except subprocess.TimeoutExpired:
        return {"code": None, "output": f"timed out after {timeout}s"}
    except OSError as e:
        return {"code": None, "output": str(e)}


def as_json(result):
    try:
        return json.loads(result["output"])
    except (ValueError, KeyError):
        return None


def status_summary(target):
    result = run(["tailscale", "status", "--json"])
    data = as_json(result)
    if not isinstance(data, dict):
        return result
    fields = ("HostName", "TailscaleIPs", "Online", "Active", "CurAddr", "Relay",
              "PeerRelay", "RxBytes", "TxBytes", "LastHandshake")
    peers = [p for p in data.get("Peer", {}).values()
             if target in (p.get("TailscaleIPs") or []) or p.get("HostName") == "carbon"]
    return {"state": data.get("BackendState"), "health": data.get("Health"),
            "exit_node": data.get("ExitNodeStatus"),
            "self": {k: data.get("Self", {}).get(k) for k in fields},
            "peers": [{k: p.get(k) for k in fields} for p in peers]}


def mosh_sessions():
    # Never read process environments: MOSH_KEY is stored there.
    result = run(["ps", "-eo", "pid=,comm=,args="])
    return [line.strip() for line in result["output"].splitlines()
            if re.match(r"\s*\d+\s+mosh-client\s", line)]


def resolve(host):
    result = run(["getent", "ahosts", host], 5)
    if result["code"] != 0:
        return result
    return sorted({line.split()[0] for line in result["output"].splitlines() if line.split()})


def wifi_snapshot(interfaces):
    return {name: {"link": run(["iw", "dev", name, "link"]),
                   "power_save": run(["iw", "dev", name, "get", "power_save"])}
            for name in interfaces}


REMOTE_REPORT = r'''
import json, subprocess, shutil, os, re
def run(args):
    if not shutil.which(args[0]): return {"code":None,"output":"not installed"}
    try:
        p=subprocess.run(args,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,
                         text=True,timeout=12,env={**os.environ,"LC_ALL":"C"})
        text=re.sub(r"(https?://)[^/\s@]+@",r"\1[credentials removed]@",p.stdout)
        return {"code":p.returncode,"output":text.strip()}
    except Exception as e:return {"code":None,"output":str(e)}
print(json.dumps({"uptime":run(["uptime"]), "cpu_count":os.cpu_count(),
 "load_and_scheduling":run(["vmstat","1","3"]),
 "addresses":run(["ip","-brief","address"]),
 "routes":run(["ip","route"]),
 "netcheck":run(["tailscale","netcheck"]),
 "tailnet":run(["tailscale","status"]),
 "udp_sockets":run(["ss","-uapn"]),
 "mosh_version":run(["mosh-server","--version"])}))
'''


def ping(target, size):
    result = run(["ping", "-n", "-c", "1", "-W", "2", "-s", str(size), target], 4)
    match = re.search(r"time[=<]([\d.]+)\s*ms", result["output"])
    return {"ok": result["code"] == 0, "rtt_ms": float(match[1]) if match else None,
            **result}


def tailnet_ping(target):
    result = run(["tailscale", "ping", "--c", "1", "--timeout", "2s",
                  "--until-direct=false", target], 4)
    match = re.search(r"via (.+?) in ([\d.]+)ms", result["output"])
    return {"path": match[1] if match else None,
            "rtt_ms": float(match[2]) if match else None, **result}


def summarize(samples):
    lines = []
    for key in ("gateway", "carbon", "precisiontower", "precisiontower_large"):
        values = [s[key] for s in samples if key in s]
        if not values:
            continue
        failures = sum(not p["ok"] for p in values)
        rtts = sorted(p["rtt_ms"] for p in values if p["rtt_ms"] is not None)
        timings = (f"; RTT median {rtts[len(rtts)//2]:.1f} ms, max {max(rtts):.1f} ms"
                   if rtts else "")
        lines.append(f"{key}: {failures}/{len(values)} probes failed{timings}")
    paths = [s.get("tailnet_path", {}).get("path") for s in samples]
    known = [p for p in paths if p]
    lines.append("Tailscale paths observed: " + (", ".join(sorted(set(known))) or "none"))
    lines.append(f"Tailscale path probes without a reply: {sum(p is None for p in paths)}/{len(paths)}")
    outage = None
    for sample in samples:
        if not sample.get("precisiontower", {}).get("ok", True):
            outage = outage or sample["time"]
        elif outage:
            lines.append(f"Observed precisiontower outage: {outage} through recovery at {sample['time']}")
            outage = None
    if outage:
        lines.append(f"Precisiontower outage started at {outage}; no recovery observed before monitoring ended")
    lines.append("Interpretation:")
    lines.append("  Gateway loss/spikes: investigate Wi-Fi or the local router; ICMP may be deprioritized.")
    lines.append("  Gateway/carbon healthy, precisiontower poor: investigate the remote path or host.")
    lines.append("  Small packets healthy, 1200-byte packets fail: investigate MTU or size-sensitive filtering.")
    lines.append("  Relay/direct changes near a pause: investigate NAT/path instability.")
    lines.append("  ICMP probes do not establish that Mosh's particular UDP port is permitted.")
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--duration", type=float, default=120, help="seconds to monitor (default: 120)")
    parser.add_argument("--interval", type=float, default=2, help="seconds between rounds (default: 2)")
    parser.add_argument("--target", default="100.64.0.2", help="precisiontower's Tailscale address")
    parser.add_argument("--remote-host", default="fli@precisiontower", help="SSH login for remote snapshot")
    parser.add_argument("--carbon", default="100.64.0.1")
    parser.add_argument("--gateway", help="override the gateway detected from the default route")
    parser.add_argument("--skip-remote", action="store_true", help="omit SSH diagnostics")
    parser.add_argument("--output", type=Path, help="new directory for reports (default: private /tmp directory)")
    args = parser.parse_args()
    if not all(math.isfinite(x) for x in (args.duration, args.interval)) or args.duration <= 0 or args.interval < 1:
        parser.error("duration must be positive and interval at least 1 second")
    for value in (args.target, args.carbon, args.remote_host):
        if value.startswith("-"):
            parser.error("host arguments cannot begin with '-' ")
    if args.output:
        args.output.mkdir(mode=0o700, parents=True, exist_ok=False)
        folder = args.output
    else:
        folder = Path(tempfile.mkdtemp(prefix="precisiontower-network-"))
    print(f"Reports: {folder}", flush=True)
    journal_since = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
    wifi = [p.name for p in Path("/sys/class/net").iterdir() if (p / "wireless").exists()]
    defaults = run(["ip", "-j", "-4", "route", "show", "default"])
    routes = as_json(defaults) or []
    gateway = args.gateway or next((r.get("gateway") for r in routes if r.get("gateway")), None)
    snapshot = {"time": now(), "target": args.target, "gateway": gateway,
                "resolution": {h: resolve(h) for h in ("precisiontower", "hs.stat.ac.cn")},
                "mosh_sessions": mosh_sessions(), "tailnet": status_summary(args.target),
                "wifi": wifi_snapshot(wifi), "commands": {}}
    commands = {"addresses": ["ip", "-brief", "address"],
                "default_routes": ["ip", "route"], "ipv6_routes": ["ip", "-6", "route"],
                "target_route": ["ip", "route", "get", args.target],
                "netcheck": ["tailscale", "netcheck"],
                "udp_sockets": ["ss", "-uapn"], "versions": ["tailscale", "version"]}
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
        futures = {pool.submit(run, cmd, 15): label for label, cmd in commands.items()}
        if not args.skip_remote:
            remote_cmd = ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
                          "-o", "StrictHostKeyChecking=yes", args.remote_host,
                          "python3 -c " + shlex.quote(REMOTE_REPORT)]
            futures[pool.submit(run, remote_cmd, 25)] = "remote"
        for future in concurrent.futures.as_completed(futures):
            label, result = futures[future], future.result()
            if label == "remote":
                snapshot["remote"] = as_json(result) or result
            else:
                snapshot["commands"][label] = result
    (folder / "snapshot.json").write_text(json.dumps(snapshot, indent=2) + "\n")
    print("Mosh sessions: " + ("; ".join(snapshot["mosh_sessions"]) or "none found"), flush=True)
    print(f"Monitoring for {args.duration:g}s. Press Enter when a freeze happens; Ctrl+C stops early.", flush=True)
    samples, lock, stopped = [], threading.Lock(), threading.Event()
    log = (folder / "samples.jsonl").open("w", buffering=1)

    def write(event):
        with lock:
            if not log.closed:
                log.write(json.dumps(event) + "\n")

    def marks():
        while not stopped.is_set():
            if not sys.stdin.readline():
                break
            if stopped.is_set():
                break
            write({"time": now(), "event": "user_mark"})
            print("Freeze marked at " + now(), flush=True)

    if sys.stdin.isatty():
        threading.Thread(target=marks, daemon=True).start()
    start = time.monotonic()
    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
            while time.monotonic() - start < args.duration:
                tick = time.monotonic()
                tasks = {"precisiontower": pool.submit(ping, args.target, 56),
                         "precisiontower_large": pool.submit(ping, args.target, 1200),
                         "carbon": pool.submit(ping, args.carbon, 56),
                         "tailnet_path": pool.submit(tailnet_ping, args.target)}
                if gateway:
                    tasks["gateway"] = pool.submit(ping, gateway, 56)
                sample = {"time": now(), "event": "sample"}
                sample.update({key: future.result() for key, future in tasks.items()})
                sample["wifi"] = wifi_snapshot(wifi)
                sample["tailnet"] = status_summary(args.target)
                sockets = run(["ss", "-uapn"])
                sample["mosh_sockets"] = ("\n".join(line for line in sockets["output"].splitlines()
                                                    if "mosh-client" in line)
                                           if sockets["code"] == 0 else sockets)
                samples.append(sample)
                write(sample)
                probes = " ".join(f"{k}={v['rtt_ms']}ms" if v["ok"] else f"{k}=FAIL"
                                  for k, v in sample.items() if k in tasks and k != "tailnet_path")
                print(f"{sample['time']} {probes} path={sample['tailnet_path']['path']}", flush=True)
                time.sleep(max(0, min(args.interval - (time.monotonic() - tick),
                                      args.duration - (time.monotonic() - start))))
    except KeyboardInterrupt:
        print("\nStopped.", flush=True)
    finally:
        stopped.set()
        with lock:
            log.close()
    final = {"time": now(), "tailnet": status_summary(args.target), "mosh_sessions": mosh_sessions()}
    if not args.skip_remote:
        # Read only; permission failures are retained for users without journal access.
        log_source = REMOTE_REPORT.split("print(json.dumps(", 1)[0] + '''
import sys
print(json.dumps({"tailscaled":run(["journalctl","-u","tailscaled","--since",sys.argv[1],"--no-pager","-n","300"]),
 "kernel":run(["journalctl","-k","--since",sys.argv[1],"--no-pager","-n","100"]),
 "network_manager":run(["journalctl","-u","NetworkManager","--since",sys.argv[1],"--no-pager","-n","100"])}))
'''
        print("Collecting precisiontower logs for the capture window...", flush=True)
        result = run(["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=5",
                      "-o", "StrictHostKeyChecking=yes", args.remote_host,
                      "python3 -c " + shlex.quote(log_source) + " " + shlex.quote(journal_since)], 25)
        final["remote_logs"] = as_json(result) or result
    (folder / "final.json").write_text(json.dumps(final, indent=2) + "\n")
    summary = summarize(samples)
    (folder / "summary.txt").write_text(summary)
    print(summary + f"Reports saved in {folder}")


if __name__ == "__main__":
    main()
