## Summary
Fixed the **retry** loop in `worker.go` — see [PR 42](https://bitbucket.org/acme/api/pull-requests/42).

- top level
  - nested **bold `code`** item
* star bullet
1. first
2. second

> quoted line

```go
func main() {}
```

| Case | Before | After |
|------|--------|-------|
| cold | 120ms | 80ms |
| warm | — | 10ms |

---
plain tail
