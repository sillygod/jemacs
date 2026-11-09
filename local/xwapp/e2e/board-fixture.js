// The Board as Emacs sends it (ghostherd-board--patch: the sidecar's
// herd_board with the forge's PR states and the Inbox's waits on the user
// added), for the Board tab's tests and screenshots.  Made-up work: five
// projects, ten items, every status, folded and open.
const now = Date.now();
const min = 60e3;
const BB = (repo, id) => "https://bitbucket.org/acme/" + repo + "/pull-requests/" + id;
const pr = (repo, id, state, title, extra) => Object.assign({
  host: "bitbucket", owner: "acme", repo, id, state, title, dest: "master", url: BB(repo, id), draft: false, unsure: false,
  own: true, of: "", first: now - 300 * min, last: now - 30 * min }, extra || {});
const card = (id, status, title, extra) => Object.assign({
  id, status, title, named: false, labels: [], asks: 2, open: 0, fyi: 0, acks: 0, relays: 0, failed: 0,
  first: now - 400 * min, last: now - 60 * min, agents: [], projects: [], cross: false, prs: [], links: [], jingOf: [],
  needs: [], deploys: [], unanswered: null,
  block: { kind: "answered", ask: "aa000000", from: "", to: "", since: now - 60 * min, head: "" }, closed: null }, extra || {});

const CARDS = [
  card("c1a2b3", "live", "結帳頁改用新的稅率表", {
    asks: 6, open: 2, fyi: 1, acks: 1, first: now - 180 * min, last: now - 4 * min, cross: true,
    agents: ["lead-shop", "lead-billing", "taxaudit", "cartfix"], projects: ["shop", "billing"], labels: ["CU-86b2k1"],
    prs: [pr("shop", 412, "OPEN", "checkout: tax table v2", { of: "c1a2b3", dest: "production", opened: { ask: "aa11aa11", at: now - 150 * min } }),
          pr("billing", 88, "MERGED", "tax API returns region", { of: "c1a2b3", merged: now - 100 * min,
            deployed: { at: now - 80 * min, text: "billing #88 → production 三站" } })],
    deploys: [{ id: 21, kind: "deploy", text: "billing #88 → production 三站", agent: "lead-billing", ask: "1a1a1a1a", at: now - 80 * min }],
    block: { kind: "open", ask: "1c1c1c1c", from: "lead-billing", to: "taxaudit", since: now - 9 * min, status: "delivered",
             working: true, head: "盤點各區的稅率差異，寫成一張對照表" } }),
  card("d4e5f6", "live", "iOS 推播在背景收不到", {
    asks: 3, open: 1, first: now - 70 * min, last: now - 12 * min, agents: ["lead-mobile", "pushfix"], projects: ["mobile"],
    block: { kind: "open", ask: "2d2d2d2d", from: "lead-mobile", to: "pushfix", since: now - 12 * min, status: "queued",
             working: false, head: "查 APNs 的 content-available 有沒有帶上" } }),
  card("e7f8a9", "live", "ops-helm 同步三站的 values", {
    asks: 4, open: 1, first: now - 95 * min, last: now - 20 * min, cross: true, agents: ["lead-infra", "lead-ops-helm", "helmsync"],
    projects: ["infra", "ops-helm"], links: ["b1c2d3"],
    prs: [pr("ops-helm", 58, "OPEN", "values: sync staging/prod", { of: "e7f8a9" }),
          pr("ops-helm", 57, "OPEN", "rotate staging certs", { own: false, of: "b1c2d3" })],
    block: { kind: "open", ask: "3e3e3e3e", from: "lead-infra", to: "lead-ops-helm", since: now - 20 * min, status: "delivered",
             working: false, head: "三站的 values 跟叢集對一次，差異列出來" } }),
  card("b1c2d3", "jing", "staging 憑證輪換", {
    asks: 4, first: now - 240 * min, last: now - 35 * min, agents: ["lead-infra", "certrot"], projects: ["infra"], jingOf: ["question:lead-infra:1"],
    needs: [{ id: 11, kind: "need", text: "在今晚的維護時段，照順序逐個 namespace 跑 rotate.sh", agent: "lead-infra", ask: null, at: now - 35 * min }],
    prs: [pr("ops-helm", 57, "OPEN", "rotate staging certs", { of: "b1c2d3" })],
    block: { kind: "answered", ask: "4f4f4f4f", from: "certrot", to: "lead-infra", since: now - 35 * min, head: "rotate.sh 寫好了，要 Jing 照順序逐個 namespace 跑" } }),
  card("f0a1b2", "tail", "購物車重複扣庫存", {
    asks: 5, acks: 2, first: now - 600 * min, last: now - 150 * min, agents: ["lead-shop", "cartfix"], projects: ["shop"], labels: ["CU-86b2k9"],
    prs: [pr("shop", 409, "OPEN", "cart: idempotent stock hold", { of: "f0a1b2" })],
    block: { kind: "answered", ask: "5a5a5a5a", from: "cartfix", to: "lead-shop", since: now - 150 * min, head: "#409 改好，測試 212 項全過，等 review" } }),
  card("a3b4c5", "quiet", "billing 對帳腳本加 dry-run", {
    first: now - 500 * min, last: now - 300 * min, agents: ["lead-billing", "taxaudit"], projects: ["billing"],
    prs: [pr("billing", 90, "MERGED", "reconcile: --dry-run", { of: "a3b4c5" })],
    block: { kind: "answered", ask: "6b6b6b6b", from: "taxaudit", to: "lead-billing", since: now - 300 * min, head: "#90 merge 了" } }),
  card("c6d7e8", "orphan", "整理 nginx 的 rate limit 設定", {
    asks: 3, first: now - 700 * min, last: now - 420 * min, agents: ["lead-infra", "ratefix"], projects: ["infra"],
    needs: [{ id: 12, kind: "need", text: "三個方案選一個：全站、只 API、只登入", agent: "lead-infra", ask: "7c7c7c7c", at: now - 420 * min }],
    block: { kind: "answered", ask: "7c7c7c7c", from: "ratefix", to: "lead-infra", since: now - 420 * min, head: "列好三個方案，等 lead 決定" } }),
  card("9a8b7c", "quiet", "mobile 要 billing 的退款 webhook", {
    asks: 3, relays: 1, cross: true, first: now - 380 * min, last: now - 260 * min, agents: ["lead-billing", "lead-shop", "lead-mobile"],
    projects: ["billing", "shop", "mobile"],
    block: { kind: "answered", ask: "8d8d8d8d", from: "lead-mobile", to: "lead-shop", since: now - 260 * min, head: "收到格式了，下週接" } }),
  card("0f1e2d", "done", "更新 README 的部署步驟", {
    named: true, first: now - 800 * min, last: now - 640 * min, agents: ["lead-shop", "docsbot"], projects: ["shop"], closed: now - 600 * min,
    block: { kind: "answered", ask: "9e9e9e9e", from: "docsbot", to: "lead-shop", since: now - 640 * min, head: "README 改好了" } }),
  card("3c4d5e", "done", "試跑新的 CI runner", {
    failed: 1, first: now - 900 * min, last: now - 700 * min, agents: ["lead-infra", "runnerx"], projects: ["infra"], closed: now - 690 * min,
    block: { kind: "cancelled", ask: "0f0f0f0f", from: "runnerx", to: "lead-infra", since: now - 700 * min, head: "cancelled" } }),
];

const WAITS = [
  { ask: "1a1a1a1a", item: "c1a2b3", from: "lead-shop", to: "lead-billing", fromProj: "shop", toProj: "billing", kind: "handoff",
    status: "delivered", since: now - 14 * min, working: true, under: null, head: "billing 的稅率 API 要多回傳 region 欄位" },
  { ask: "1c1c1c1c", item: "c1a2b3", from: "lead-billing", to: "taxaudit", fromProj: "billing", toProj: "billing", kind: "ask",
    status: "delivered", since: now - 9 * min, working: true, under: "1a1a1a1a", head: "盤點各區的稅率差異，寫成一張對照表" },
  { ask: "3e3e3e3e", item: "e7f8a9", from: "lead-infra", to: "lead-ops-helm", fromProj: "infra", toProj: "ops-helm", kind: "handoff",
    status: "delivered", since: now - 20 * min, working: false, under: null, head: "三站的 values 跟叢集對一次，差異列出來" },
  { ask: "2d2d2d2d", item: "d4e5f6", from: "lead-mobile", to: "pushfix", fromProj: "mobile", toProj: "mobile", kind: "ask",
    status: "queued", since: now - 12 * min, working: false, under: null, head: "查 APNs 的 content-available 有沒有帶上" },
];

const JING = [
  { id: "need:11", event: 11, kind: "need", tag: "需要 Jing", line: true, item: "b1c2d3", itemTitle: "staging 憑證輪換",
    title: "在今晚的維護時段，照順序逐個 namespace 跑 rotate.sh", agent: "lead-infra", at: now - 35 * min, ask: null },
  { id: "need:12", event: 12, kind: "need", tag: "需要 Jing", line: true, item: "c6d7e8", itemTitle: "整理 nginx 的 rate limit 設定",
    title: "三個方案選一個：全站、只 API、只登入", agent: "lead-infra", at: now - 420 * min, ask: "7c7c7c7c" },
  { id: "question:lead-infra:1", kind: "question", tag: "Question", agent: "lead-infra", project: "infra",
    title: "rotate.sh 要現在跑，還是等今晚的維護時段？", preview: "腳本寫好了，staging 三個 namespace 逐個跑", at: now - 30 * min, item: "b1c2d3", guess: true },
  { id: "blocked:pushfix", kind: "blocked", tag: "Permission", agent: "pushfix", project: "mobile",
    title: "Permission: run xcodebuild test", preview: "Bash: xcodebuild test -scheme App", at: now - 6 * min, item: "d4e5f6", guess: true },
];

const META = {
  counts: { asks: 34, xproj: 9, fyi: 4, relays: 1, failed: 2 },
  edges: [{ from: "shop", to: "billing", relay: false, n: 3 }, { from: "billing", to: "shop", relay: false, n: 2 },
          { from: "infra", to: "ops-helm", relay: false, n: 2 }, { from: "ops-helm", to: "infra", relay: false, n: 1 },
          { from: "shop", to: "infra", relay: false, n: 1 }, { from: "billing", to: "mobile", relay: true, n: 1 }],
  days: 1, more: true,
};

const FULL = { v: 1, base: 0, full: true, cards: CARDS, drop: [], waits: WAITS, jing: JING, meta: META };

const TIMELINE = {
  id: "c1a2b3", last: CARDS[0].last, asksOf: [],
  events: [
    { kind: "ask", at: now - 180 * min, ask: "aa11aa11", from: "lead-shop", to: "cartfix", head: "結帳頁換成新的稅率表，先列出要改的地方", fold: null, relay: false, status: "answered", key: true },
    { kind: "answer", at: now - 150 * min, ask: "aa11aa11", from: "cartfix", to: "lead-shop", head: "要改 4 個檔，billing 的 API 少一個 region 欄位", fold: null, auto: false, key: false },
    { kind: "open", at: now - 150 * min, ask: "aa11aa11", from: "cartfix", to: "", head: "shop #412 開好", key: true, pr: "bitbucket:acme/shop#412" },
    { kind: "ask", at: now - 140 * min, ask: "bb22bb22", from: "lead-shop", to: "cartfix", head: "#412 開好之後回一句收到即可", fold: "fyi", relay: false, status: "answered", key: false },
    { kind: "answer", at: now - 139 * min, ask: "bb22bb22", from: "cartfix", to: "lead-shop", head: "收到", fold: "ack", auto: false, key: false },
    { kind: "need", at: now - 130 * min, ask: "cc33cc33", from: "lead-shop", to: "Jing", head: "同意先上 billing #88", key: true, state: "done", event: 9 },
    { kind: "deploy", at: now - 80 * min, ask: "1a1a1a1a", from: "lead-billing", to: "", head: "billing #88 → production 三站", key: true, event: 21 },
    { kind: "handoff", at: now - 14 * min, ask: "1a1a1a1a", from: "lead-shop", to: "lead-billing", head: "billing 的稅率 API 要多回傳 region 欄位", fold: null, relay: false, status: "delivered", key: true },
    { kind: "ask", at: now - 9 * min, ask: "1c1c1c1c", from: "lead-billing", to: "taxaudit", head: "盤點各區的稅率差異，寫成一張對照表", fold: null, relay: false, status: "delivered", key: true },
  ],
};

module.exports = { FULL, CARDS, WAITS, JING, META, TIMELINE, now };
