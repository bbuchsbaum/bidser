# AGENTS.md

## Oracle-First Rule

This package participates in the EcoOracle ecosystem.

Before searching code across ecosystem repos or writing exploratory
scripts:

1.  Break the task into 2-6 “How do I …?” subquestions.
2.  Query `eco_howto(query)` for each subquestion.
3.  Query `eco_symbol("pkg::fn")` for key functions.
4.  Stitch returned `recipe` snippets before falling back to source
    spelunking.
5.  Use `eco_packages()` and `eco_where_used()` for discovery and
    cross-package usage.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work
is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1.  **File issues for remaining work** - Create issues for anything that
    needs follow-up

2.  **Run quality gates** (if code changed) - Tests, linters, builds

3.  **Update issue status** - Close finished work, update in-progress
    items

4.  **PUSH TO REMOTE** - This is MANDATORY:

    ``` bash
    git pull --rebase
    bd sync
    git push
    git status  # MUST show "up to date with origin"
    ```

5.  **Clean up** - Clear stashes, prune remote branches

6.  **Verify** - All changes committed AND pushed

7.  **Hand off** - Provide context for next session

**CRITICAL RULES:** - Work is NOT complete until `git push` succeeds -
NEVER stop before pushing - that leaves work stranded locally - NEVER
say “ready to push when you are” - YOU must push - If push fails,
resolve and retry until it succeeds
