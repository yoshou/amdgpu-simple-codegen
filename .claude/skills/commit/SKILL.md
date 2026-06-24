---
name: commit
description: Create a git commit for the current changes using a one-line Conventional Commits message, committing directly to the main branch. Use whenever the user asks to commit, save, or check in their work — e.g. "commit this", "コミットして", "make a commit".
---

# Commit

Create a single commit for the current working changes with a clean, one-line Conventional Commits message, directly on the `main` branch (do not create or switch branches).

## Workflow

1. **Inspect the changes.** Run `git status` and `git diff` (plus `git diff --staged` if anything is already staged) in parallel to understand what changed and why. If nothing is staged, the changes still need staging in step 3.

2. **Decide the message.** Summarize the actual intent of the change as one Conventional Commits line:

   ```
   <type>: <description>
   ```

   - `type` is one of: `feat`, `fix`, `refactor`, `perf`, `docs`, `test`, `build`, `chore`, `style`, `ci`.
   - Keep it to a single line, imperative mood, lowercase description, no trailing period. Aim for ≤ 72 characters.
   - Do not add a scope — just `<type>: <description>`.
   - Describe what the change accomplishes, not how. Don't add a body unless the user explicitly asks for one; the point is a simple one-liner.

3. **Stage and commit on main.** Confirm you are on `main` (`git branch --show-current`); the policy is to commit directly to main, so do not branch. Stage the relevant files (`git add`) and commit with the single-line message:

   ```bash
   git commit -m "<type>: <description>"
   ```

   The commit message is exactly one Conventional Commits line — no body, and **no `Co-Authored-By` or any other trailer**.

4. **Report.** Show the resulting commit (`git log -1 --oneline`) so the user can confirm.

## Notes

- Only commit when the user asks. Don't push unless they ask separately.
- If the changes are clearly unrelated (touching multiple independent concerns), point that out and ask whether to split them rather than forcing one message.
- Never commit files the user didn't intend to (build artifacts, archives, editor folders). If `git status` shows suspicious untracked files, flag them before staging everything.

## Examples

**Example 1**
Changes: fixed an array-concatenation type error so the crate builds again.
Message: `fix: resolve register array concatenation build error`

**Example 2**
Changes: added a new IR-to-vreg translation pass.
Message: `feat: add IR translator pass`

**Example 3**
Changes: reformatted code, no behavior change.
Message: `style: apply formatter`
