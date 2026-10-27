# Precisiontower connection diagnostics

Run on ti as your normal user while keeping Mosh open in another terminal:

```sh
python3 ~/.emacs.d/scripts/inspect-precisiontower.py --duration 300
```

Press Enter in the diagnostic terminal when Mosh freezes. The mark and probe
timestamps are written to `samples.jsonl`. Ctrl+C ends monitoring and writes
a summary. SSH diagnostics use keys, do not prompt, and require an already
trusted host key. No sudo is needed. If SSH is unavailable, the local checks
continue. `--skip-remote` omits the remote snapshot.

The script records gateway and carbon pings alongside small and 1200-byte
precisiontower pings, Tailscale direct/relay paths, Wi-Fi signal and power saving,
DNS resolution, UDP socket state, and active Mosh destinations. It also collects
a remote snapshot of uptime, networking and Tailscale netcheck over SSH.
It does not read Mosh keys or process environments, change settings, or start
a new Mosh server. Reports contain hostnames, IP addresses and Wi-Fi identifiers;
the default report directory under `/tmp` is private to your user.

`snapshot.json` contains initial diagnostics; `samples.jsonl` is the time series;
`final.json` captures the final path state and precisiontower's Tailscale, kernel
and NetworkManager logs during the capture window; `summary.txt` reports loss,
latency and observed outage windows. Remote log access depends on account permissions.
Unavailable tools and timed-out checks are recorded as errors rather than stopping
the other checks. Install `iw` if Wi-Fi signal checks say it is unavailable.

Example with a chosen report directory (must not already exist):

```sh
python3 ~/.emacs.d/scripts/inspect-precisiontower.py \
    --duration 300 --interval 2 --output /tmp/precisiontower-evening
```

The default target is `100.64.0.2`, carbon is `100.64.0.1`, and the gateway is
detected from the main IPv4 default route. Use `--target`, `--carbon`, `--gateway`
and `--remote-host` to override these. `mosh fli@precisiontower` currently resolves
to `100.64.0.2`; the script logs running `mosh-client` arguments to verify this.

Gateway loss or latency spikes can indicate local Wi-Fi problems. If the gateway
and carbon remain healthy but precisiontower degrades, investigate the Internet
path or precisiontower. ICMP may be filtered or deprioritized; packet loss in this
report is not by itself proof of application packet loss. A passing ICMP test does
not establish that Mosh's individual UDP port is permitted. Wi-Fi power saving is
recorded, not automatically blamed or disabled. Probe traffic can keep a path
active, so this monitor can mask a problem that happens only after inactivity.

Tailscale can carry traffic directly to precisiontower even when Headscale is
nearby on the same Wi-Fi. Headscale's location does not determine the path used
by an established Mosh connection.
