# Smartabase International Scouting Dashboard

R Shiny application for the Miami Marlins international scouting department. Integrates Smartabase and SQL Server (Lagoon) to centralize player evaluation workflows.

## Features

- **Drag-to-Rank Interface** — Sortable player ranking with drag-and-drop reordering
- **Consensus Review Board** — Aggregated evaluator rankings for cross-scout alignment
- **Follow List Management** — Track and manage prospect follow lists
- **Smartabase Profile Creation** — Create new player profiles directly from the app
- **Live Status Filtering** — Filter players by Status, Agent Name, Signing Bonus, and other evaluation fields
- **Email Integration** — Automated reporting via `blastula`

## Stack

- **R / Shiny**
- **Smartabase** via `smartabaseR`
- **SQL Server** (DSN: Aquarium)
- **Deployed on** Posit Connect

## Usage

Internal tool — requires Marlins network access and Smartabase credentials.
