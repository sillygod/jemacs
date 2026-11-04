// A project's room as a busy afternoon has it: a lead and three workers
// it started, each with its model, context and tokens, the tickets its
// conversation named (the lead's two dozen), a branch with its PR, and
// a conversation each -- one with a table whose first column is CJK.
// Shared by herd-room-compact.js (the checks) and shot-room.js (the
// screenshots, master's and this branch's).
const now = Date.now();
const P = "~/src/shop-api/";
const BB = "https://bitbucket.org/acme/shop-api/pull-requests/";
const NAMES = ["lead-ops", "claude", "investigator", "ipguard", "reviewer"];

const meter = (tokens, extra) => Object.assign({ model: "Opus 5.5", effort: "xhigh", tokens, window: 1000000,
  pending: false, compacted: false }, extra || {});
const t = (w) => ({ w, input: Math.round(w / 500), write: Math.round(w / 10), read: Math.round(w * 6), output: Math.round(w / 40) });
const tally = (today, week) => ({ today: t(today), week: t(week) });
const task = (id, name, status, color, done) => ({ id, name, status, color: color || "#4194f6",
  url: "https://app.clickup.com/t/" + id, error: null, loading: false, done: !!done });
const pr = (id, state, title) => ({ id, title, state, draft: false, url: BB + id, error: null });
const WORDS = ["[production] nginx 把會員入口的舊路徑關掉", "[production] 其他 service 的 token 輪替", "[web/admin] 拿掉原始碼裡的預設 token",
  "[production] API_SIGNING_KEY 換掉", "[staging] 管理介面只能 SSO 登入", "[staging] default project 收緊", "[security] 異常請求的來源追查",
  "[production] 日誌保留期延長", "[staging] 排程 bot 改走一般 JWT", "[ops] port-forward 的權限收回"];
const LEAD_TASKS = Array.from({ length: 24 }, (_, i) =>
  task("86lt" + i, WORDS[i % WORDS.length] + (i >= WORDS.length ? " (" + (i + 1) + ")" : ""),
    i % 3 ? "in progress" : "to do", i % 3 ? "#4194f6" : "#87909e"));

const agent = (name, extra) => Object.assign({
  name, kind: "claude", state: "idle", project: P, role: "", reason: "seen", notes: "", backend: "tmux", detached: false,
  manual: false, login: false, url: "", continueAt: 0, continueStopped: false, startedBy: "", background: [], subs: [],
  since: now - 3600e3, progress: null, said: null, saidAt: null, conversation: null, work: false, links: false,
  switching: false, suggestion: "" }, extra || {});

const AGENTS = [
  agent("lead-ops", { role: "lead", notes: "lead: plans the work, hands it out", since: now - 240e3, background: ["1 shell"],
    meter: meter(631000), tally: tally(14.1e6, 25.2e6),
    links: { root: P, prs: [], tasks: LEAD_TASKS } }),
  agent("claude", { since: now - 86400e3, meter: meter(182000), tally: tally(0, 1.2e6),
    links: { root: P, prs: [], tasks: [task("86sa1", "[Security][staging] 管理介面異常登入的調查", "in progress")] } }),
  agent("investigator", { startedBy: "lead-ops", since: now - 3600e3,
    notes: "staging login anomaly: read-only investigation (where from, first request, what was touched)",
    meter: meter(null, { compacted: true, tokens: null }), tally: tally(11e6, 27.9e6),
    links: { root: P, prs: [], tasks: [task("86sa1", "[Security][staging] 管理介面異常登入的調查", "in progress"),
                                        task("86sa2", "異常請求的來源追查", "in progress")] } }),
  agent("ipguard", { startedBy: "lead-ops", since: now - 26 * 60e3, notes: "staging: an ingress IP allowlist in front of the admin pages",
    meter: meter(null, { compacted: true, tokens: null }), tally: tally(8e6, 12.8e6),
    work: { project: P, root: "~/.ghostherd/worktrees/shop-api/ipguard/", branch: "fix/production-admin-ip-allowlist",
            pending: false, error: null, ahead: 1, base: "master", size: 48 * 1024,
            pr: pr(8, "MERGED", "production: an IP allowlist on the admin ingress"),
            tasks: [task("86sa1", "[Security][staging] 管理介面異常登入的調查", "in progress"),
                    task("86sa3", "[Security][staging/production] admin ingress allowlist", "complete", "#3fb950", true)] },
    links: { root: P, prs: [], tasks: [task("86sa4", "[production] 封鎖 203.0.113.7", "in progress")] } }),
  agent("reviewer", { kind: "grok", state: "working", startedBy: "lead-ops", since: now - 90e3,
    reason: "Reading ingress.yaml and the allowlist diff", notes: "reviews ipguard's allowlist PR",
    meter: false, tally: tally(2e6, 4e6) }),
];

const ts = (min) => new Date(now - min * 60e3).toISOString();
const TABLE = [
  "所以權限檢查寫的 `IsMember | IsStaff | IsService` 代表有兩條路可以進去：",
  "",
  "| 環境 | 狀態 | 說明 |",
  "|---|---|---|",
  "| production 狀態 | 沒設，所有地方都用原始碼預設值 | 有設，而且不是預設值。例如服務 A 的 `SERVICE_A_SIGNING_KEY`，跟服務 B 的 `SERVICE_B_SIGNING_KEY` 是同一把 ⚠️ " +
    "所以 staging 跟 production 用的是同一把，拿到一個就等於拿到兩個環境的 `SHARED_SERVICE_BEARER_TOKEN`，`orders_for_service`、`pending_refund_jobs` 也一樣 |",
  "| staging 環境 | 有設 | 同上，排程 bot 走的也是一般 JWT |",
  "| 本機開發 | 預設值 | 不影響 |",
  "",
  "- 前兩個（`IsMember`、`IsStaff`）走一般 JWT，這條是安全的。",
  "- **IsService** 走 `SHARED_SERVICE_BEARER_TOKEN`，這就是要先修的地方。",
].join("\n");
const PARA = (n) => Array.from({ length: n }, (_, i) =>
  "Step " + (i + 1) + ": read the `ord-` ids that the create endpoint would hand out, and the jobs that ran on their first schedule. " +
  "這段是第 " + (i + 1) + " 段的說明，夠長才會換行好幾次。").join("\n\n");
const conv = (name, table) => [
  { role: "user", kind: "prompt", text: "那跟共用的 service token 的關聯是？", ts: ts(30) },
  { role: "assistant", text: PARA(3), ts: ts(29) },
  { role: "user", kind: "prompt", text: "好，有人處理了。另外我好奇這把 token 的權限可以做到什麼？", ts: ts(20) },
  { role: "assistant", text: (table ? TABLE + "\n\n" : "") + PARA(6), ts: ts(2) },
];
const CONVS = Object.fromEntries(NAMES.map((n) => [n, conv(n, n === "lead-ops" || n === "investigator")]));

// The herd with the first N agents of the room.
// A copy each time: a check that changes an agent changes only its own.
const herd = (n) => ({ agents: JSON.parse(JSON.stringify(AGENTS.slice(0, n || 4))), asks: [], hooks: { status: {}, list: [] }, usage: [],
  kinds: ["claude", "agy", "grok"], work: [] });

// Open the room of P, in VIEW, and give each column its conversation.
async function openRoom(page, n, view, tap, sleep) {
  await page.evaluate((h) => { GM.showView("agents"); GM.renderHerd(h); }, herd(n));
  await sleep(120);
  await tap('.pgroup[data-project="' + P + '"] [data-act="a:room"]');
  await sleep(150);
  if (view) {
    await tap('.room [data-act="a:room-view"][data-view="' + view + '"]');
    await sleep(150);
  }
  for (const a of AGENTS.slice(0, n || 4)) {
    await page.evaluate((c) => GM.setConversation(c),
      { name: a.name, path: "/x/" + a.name + ".jsonl", cli: a.kind, earlier: false, entries: CONVS[a.name] });
  }
  await sleep(200);
}

module.exports = { P, AGENTS, NAMES, CONVS, TABLE, herd, openRoom };
