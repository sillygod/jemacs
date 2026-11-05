// Kicking the path (xwapp.el): a phone whose tailscale path is relayed
// -- 15 KB/s, 300 ms, as through DERP Hong Kong -- has Emacs ping it until
// it is direct, on its page's first request and when it wakes; once
// direct, the link here turns the LAN's.  tailscale is played
// (E2E_DIR/tailscale, herd-room.el): its ping pongs via DERP, then
// direct after DIRECT seconds, and says so to the test, which turns the
// link then.  What this shows is Emacs's side -- when it kicks, that
// Emacs never waits for it, what waits and what does not, what the
// timings say.  The real network is not here: how soon a real phone's
// path turns direct, and how fast direct is, only real use says (the
// timings now say the path at a moment's start and end, and the kick).
//
// Run with the hold off (as by default) and on (E2E_HOLD=4): the room
// opened right after a wake, its newest at once either way, its rest on
// the relay or after the path turned.
const fs = require("fs");
const path = require("path");
const { start, watch, phoneEyes, BOX, PHONE, until, sleep, log } = require("./appslib");

const results = [];
const check = (n, ok, d) => { results.push(!!ok); console.log((ok ? "PASS " : "FAIL ") + n + (ok || d === undefined ? "" : "  — " + JSON.stringify(d))); };

const RELAY = { kbps: 120, rtt: 300 };
const LAN = { kbps: 40000, rtt: 20 };
const PHONE_IP = "100.94.115.122";
const PROJECT = "/Users/someone/src/fox/";
const s1 = (ms) => (ms / 1000).toFixed(2) + " s";

// tailscale, played: `ping' pongs via DERP at once, then direct after
// the seconds in DIR/direct-after -- touching DIR/direct, for the test to
// turn the link -- or never ("never"), as tailscale ping says it.
function playTailscale(dir) {
  const script = path.join(dir, "tailscale");
  fs.writeFileSync(script, [
    "#!/bin/sh",
    'D="$(dirname "$0")"',
    'echo "$@" >> "$D/ts-calls"',
    '[ "$1" = ping ] || exit 1',
    "sleep 0.3",
    `echo "pong from iphone171 (${PHONE_IP}) via DERP(hkg) in 380ms"`,
    'after=$(cat "$D/direct-after" 2>/dev/null || echo never)',
    'if [ "$after" = never ]; then sleep 3; exit 1; fi',
    'sleep "$after"',
    'touch "$D/direct"',
    `echo "pong from iphone171 (${PHONE_IP}) via 192.168.50.104:41641 in 19ms"`,
    "",
  ].join("\n"));
  fs.chmodSync(script, 0o700);
  fs.writeFileSync(path.join(dir, "direct-after"), "1\n");
}

const pings = (dir) => { try { return fs.readFileSync(path.join(dir, "ts-calls"), "utf8").trim().split("\n").filter(Boolean); } catch (_) { return []; } };
const timings = (dir) => { try { return fs.readFileSync(path.join(dir, "timings.jsonl"), "utf8").trim().split("\n").map(JSON.parse); } catch (_) { return []; } };

async function run(hold) {
  const W = (n) => "hold " + (hold ? hold + " s" : "off") + ": " + n;
  console.log(`\n-- the hold ${hold ? hold + " s" : "off"}`);
  const link = Object.assign({}, RELAY);
  const s = await start({ el: "herd-room.el", link, peer: PHONE_IP, setup: playTailscale,
                          env: { E2E_TAILSCALE: "1", E2E_HOLD: String(hold) } });
  // The path turns direct when the played ping says so: the link with it.
  let directAt = 0;
  let watching = true;
  (async () => {
    while (watching) {
      if (fs.existsSync(path.join(s.dir, "direct"))) {
        fs.rmSync(path.join(s.dir, "direct"), { force: true });
        directAt = Date.now();
        s.setLink(LAN);
      }
      await sleep(30);
    }
  })();
  try {
    const ctx = await s.browser.newContext(PHONE);
    await phoneEyes(ctx);
    const page = await ctx.newPage();
    const errors = [];
    watch(page, errors);

    // ---- Opened on the relay ---------------------------------------------
    const t0 = Date.now();
    await page.goto(BOX + "/" + s.secretOf("herd") + "/ghostherd/ui/index.html");
    await page.waitForFunction(() => window.GM && window.XW);
    const first = pings(s.dir);
    check(W("the page's first request has Emacs ping the phone, until direct"),
          first.length === 1 && first[0] === "ping --until-direct --c 15 --timeout 2s " + PHONE_IP, first);
    await page.tap('.tab[data-tab="agents"]');
    await until(() => page.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 60000);
    console.log(`   opened: direct ${directAt ? s1(directAt - t0) : "never"} after the first request`);
    let open = null;
    await until(() => (open = timings(s.dir).find((r) => r.kind === "open")), 15000);
    console.log(`   its timing: ${open ? open.d.toFixed(2) + " s, " + (open.path0 || "?") + " → " + (open.path1 || "?") +
                ", kick " + JSON.stringify(open.kick) : "none"}`);
    check(W("its timing says the path then and after, and the kick"),
          open && open.path0 === "relay hkg" && open.path1 === "direct" && open.kick && open.kick.how === "direct" &&
          open.kick.direct > 0.9 && open.kick.direct < 3, open);
    if (open && open.slow) {
      await page.tap('.tab[data-tab="log"]');
      const said = await until(() => page.evaluate(() => /slow open [\d.]+ s: .* · relay hkg → direct(, LAN)? · kicked: direct in [\d.]+ s/.test(document.body.textContent)), 15000);
      check(W("in the Log: relay → direct, kicked, how soon"), said);
      await page.tap('.tab[data-tab="agents"]');
      await until(() => page.evaluate((p) => !!document.querySelector('.pgroup[data-project="' + p + '"]'), PROJECT), 15000);
    }

    // ---- Asleep, woken on the relay, the room opened at once -----------
    // Past the harness's kick-every (6 s) since the load's kick, a phone
    // asleep a while -- what it had under way hanging -- woken.
    await sleep(Math.max(1500, 6500 - (Date.now() - t0)));  // settled: its next held
    s.setLink(RELAY);
    directAt = 0;
    fs.writeFileSync(path.join(s.dir, "direct-after"), "2.5\n");
    await page.evaluate(() => __vis("hidden"));
    await sleep(300);
    s.nap();
    await sleep(2500);
    const tWake = Date.now();
    await page.evaluate(() => __vis("visible"));
    s.wake("hang");
    const kicked = await until(() => pings(s.dir).length === 2, 5000);
    check(W("woken, Emacs pings it again"), kicked, pings(s.dir));
    const tTap = Date.now();
    await page.tap('.pgroup[data-project="' + PROJECT + '"] [data-act="a:room"]');
    await until(() => page.evaluate(() => !!document.querySelector(".chat .cbody .cmsg")), 30000);
    const drawn = Date.now() - tTap;
    const shown = await page.$$eval(".chat .cbody .cmsg", (m) => m.length);
    // When it was whole; what of it came over the relay, before the path
    // turned: the bytes of the nexts done by then (a part held at its end
    // is drawn with the last, so the page's count would not say).
    await until(() => page.evaluate(() => document.querySelectorAll(".chat .cbody .cmsg").length >= 200), 60000);
    const whole = Date.now() - tTap;
    await until(() => directAt, 10000);
    const early = s.traffic.filter((t) => /\/next\?/.test(t.path) && t.end && t.end > tTap && t.end < directAt)
      .reduce((a, t) => a + t.bytes, 0);
    console.log(`   woken: the room's newest drawn ${s1(drawn)} (${shown} items) after the tap · direct ` +
                `${directAt ? s1(directAt - tTap) : "never"} · whole ${s1(whole)} · ${(early / 1024).toFixed(1)} KB came on the relay`);
    check(W("the room's newest come at once, on the relay, whatever the hold"), drawn < 2500 && shown > 0, { drawn, shown });
    if (hold) check(W("the rest waited for the path, and came then"), early < 15 * 1024 && whole >= directAt - tTap, { early });
    else check(W("the rest did not wait for the path"), early > 15 * 1024, { early });
    check(W("no page errors"), errors.length === 0, errors.slice(0, 4));
    await ctx.close();
    return { drawn, whole, early, direct: directAt ? directAt - tTap : null, wake: tTap - tWake };
  } finally {
    watching = false;
    await s.stop();
  }
}

(async () => {
  const off = await run(0);
  const on = await run(4);
  console.log(`\n   woken on the relay, the path direct ${s1(off.direct)} / ${s1(on.direct)} after the tap:` +
              `\n   hold off: newest ${s1(off.drawn)}, whole ${s1(off.whole)}, ${(off.early / 1024).toFixed(1)} KB of it on the relay` +
              `\n   hold 4 s: newest ${s1(on.drawn)}, whole ${s1(on.whole)}, ${(on.early / 1024).toFixed(1)} KB of it on the relay`);
  const failed = results.filter((r) => !r).length;
  console.log(failed ? `\n${failed} of ${results.length} failed` : `\nall ${results.length} passed`);
  process.exit(failed ? 1 : 0);
})().catch((e) => { console.error(e); process.exit(1); });
