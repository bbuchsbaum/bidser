# AGENTS.md

## Oracle-First Rule

This package participates in the EcoOracle ecosystem.

Before searching code across ecosystem repos or writing exploratory scripts:

1. Break the task into 2-6 "How do I ...?" subquestions.
2. Query `eco_howto(query)` for each subquestion.
3. Query `eco_symbol("pkg::fn")` for key functions.
4. Stitch returned `recipe` snippets before falling back to source spelunking.
5. Use `eco_packages()` and `eco_where_used()` for discovery and cross-package usage.
