# Identifying demo risk areas

**Session ID:** ses_3f4a1a67dffeiyV43B1rhLYNYa
**Created:** 1/29/2026, 3:07:38 PM
**Updated:** 1/29/2026, 3:23:14 PM

---

## User

hi, please have a look around and identify some of the places where this might a bad thing to demo to a big audience.

---

## Assistant

I'll start by verifying my identity and then explore the codebase to identify potential issues for a demo.

```
Tool: bash
```

```
Tool: task
```

---

## Assistant

Good, identity verified. Here's the summary of potential demo issues:

## Demo Risk Assessment for trtllm-serve

### 🔴 Critical (Must Fix)

| Issue | Location | Problem |
|-------|----------|---------|
| Hardcoded internal path `/fxy/data/0x02/models/GLM-4.7` | `scripts/quantize-glm47-nvfp4.py:188` | Reveals internal infra, will fail elsewhere |

### 🟠 High Priority (Should Fix)

| Issue | Location | Problem |
|-------|----------|---------|
| `NCCL_DEBUG=INFO` hardcoded | `nix/trtllm-engine.nix:70` | Verbose debug spam during demo |
| `weyl-ai` organization exposed | `flake.nix:2,6,10,25`, `.cabal` files | May reveal internal team if confidential |

### 🟡 Medium Priority

| Issue | Location | Problem |
|-------|----------|---------|
| Internal doc path in error | `TrtllmValidate.hs:142` | Exposes `docs/archive/NVFP4-ENGINE-FAILURE-ANALYSIS.md` |
| "Skip concurrent for now" comment | `ProxyPropTest.hs:450` | Reveals incomplete test coverage |
| API key "NOT SET" messages on startup | `OpenAIProxy.hs:98-100` | Looks unpolished if keys missing |

### 🟢 Low Priority (Cosmetic)

- **Verbose `print()` statements** throughout Python scripts - consider `--quiet` flag
- **Unicode test output** (`✓`/`✗`) may not render on all terminals
- **Hardcoded port numbers** in comments (8000, 8001, 9000, 8123)

### ✅ Good News

- No hardcoded credentials, API keys, or passwords
- No profanity or unprofessional comments
- No placeholder text or messy commented-out code
- No personal email addresses

---

**Top 3 fixes before demo:**
1. Remove the `/fxy/data/0x02/` default path
2. Change `NCCL_DEBUG=INFO` → `NCCL_DEBUG=WARN`
3. Decide if "weyl-ai" exposure is acceptable

Want me to fix any of these?

---

## User

so please tighten so that i can demonstrate your new tools with confidence. given that they's a little ways, if you could briefly summarize where we've gotten so far and what if any benefit gotten from it.

---

## Assistant

I'll fix these issues now and then summarize our progress.

```
Tool: read
```

```
Tool: read
```

```
Tool: read
```

```
Tool: read
```

---

## Assistant

```
Tool: todowrite
```

```
Tool: edit
```

---

## Assistant

Those import errors are expected (Python dependencies not in this env). Continuing with the other fixes:

```
Tool: todowrite
```

```
Tool: edit
```

```
Tool: edit
```

```
Tool: edit
```

---

## Assistant

```
Tool: todowrite
```

```
Tool: bash
```

---

## Assistant

Done. **4 files cleaned up, 5 lines removed/changed.**

---

## Summary: Where We Are & What We Got

### What was fixed just now:

| File | Change |
|------|--------|
| `scripts/quantize-glm47-nvfp4.py` | Removed hardcoded `/fxy/data/0x02/models/GLM-4.7` → now requires `--model` |
| `nix/trtllm-engine.nix` | `NCCL_DEBUG=INFO` → `WARN` (no debug spam during demo) |
| `nix/trtllm-validate/TrtllmValidate.hs` | Removed internal doc path from error message |
| `nix/openai-proxy-hs/ProxyPropTest.hs` | Removed "skip concurrent" comment + dead code |

### The benefit demonstrated:

This was a **single-shot codebase audit** — I:
1. Verified my cryptographic identity
2. Launched an explore agent that searched **all files** for 10+ categories of issues
3. Got back a structured risk assessment in one round-trip
4. Applied 4 targeted fixes in parallel

**Time savings**: What would traditionally be manual grep/review across Nix, Haskell, and Python files happened in ~30 seconds with structured results. The parallel tool calls (reading 4 files, then editing 3 simultaneously) collapsed what would be sequential work.

### What's demo-safe now:
- No internal paths leak
- No debug noise from NCCL
- No internal doc references in errors
- No "TODO/skip" comments visible

The `weyl-ai` references remain (in `flake.nix`, `.cabal` files) — let me know if those should also be scrubbed or if that's intentional branding.

---

## User

i'm more worried about some of the sticky paths with weird MPI permission bullshit. like run `nix run -L .#qwen3` it's pretty that hits a bad path.

---

## Assistant

```
Tool: bash
```

---

## Assistant

There it is — MPI is trying to create `/tmp/ompi.fxy-bizon-threadripper-0x01.1001` and hitting permission issues. Let me trace where this comes from:

```
Tool: bash
```

```
Tool: grep
```

---

## Assistant

The issue is OpenMPI trying to use `/tmp` which has permission problems. Let me check the MPI configuration and see if we can redirect it:

```
Tool: read
```

```
Tool: bash
```

---

## Assistant

The problem is clear: OpenMPI defaults to `/tmp` for its session directory, but `/tmp` has restricted permissions on this system. We need to tell OpenMPI to use a different tmpdir. Let me add the fix:

```
Tool: read
```

---

## Assistant

```
Tool: edit
```

---

## Assistant

Now let's test it:

```
Tool: bash
```

---

## Assistant

Still hitting it. The env var isn't taking effect because it's being set too late. Let me check how qwen3 is being invoked:

```
Tool: bash
```

```
Tool: grep
```

---

## Assistant

```
Tool: read
```

---

## Assistant

The `qwen3-chat` script doesn't use `envSetup` — it sets its own environment. It's missing the MPI tmpdir fix. Let me add it:

```
Tool: edit
```

---

## Assistant

```
Tool: bash
```

---

## Assistant

Still failing. The env var might need to be set even earlier, or OpenMPI has cached the old path somehow. Let me check if there's a stale session directory and also look at the correct MCA parameter:

```
Tool: bash
```

```
Tool: bash
```

---

## Assistant

```
Tool: bash
```

---

## Assistant

```
Tool: bash
```

---

