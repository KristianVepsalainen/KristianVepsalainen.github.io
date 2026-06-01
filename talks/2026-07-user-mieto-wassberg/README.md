# One Hundredth of a Second — useR! 2026

Bayesian forward simulation of aerodynamic drag at the 1980 Olympic men's 15 km
cross-country skiing race. Slides for a 15–20 minute talk at useR! 2026.

## Files

- `mieto-wassberg-useR2026.qmd` — the slide source (Quarto + revealjs)
- `custom.scss` — theme

## Render

```bash
quarto render mieto-wassberg-useR2026.qmd
```

This produces `mieto-wassberg-useR2026.html`, openable in any browser.
Press `s` during the talk to open speaker notes.
Press `?` for the full revealjs keyboard help.

Requirements: Quarto ≥ 1.4, R with `tidyverse` and `scales`.

## Timing (target: 17 min + 3 min Q&A)

| # | Slide                          | Time   | Cumulative |
|--:|--------------------------------|-------:|-----------:|
|  1 | Title                         | 0:20   |  0:20      |
|  2 | The race                      | 1:30   |  1:50      |
|  3 | Not a history talk            | 1:00   |  2:50      |
|  4 | Why this is a useR talk       | 1:00   |  3:50      |
|  5 | The plan                      | 0:45   |  4:35      |
|  6 | What we know                  | 1:00   |  5:35      |
|  7 | The physics                   | 1:30   |  7:05      |
|  8 | Anchoring v to splits         | 1:30   |  8:35      |
|  9 | Priors visualized             | 1:00   |  9:35      |
| 10 | The model in R                | 2:00   | 11:35      |
| 11 | The result (posterior plot)   | 1:30   | 13:05      |
| 12 | What it says / doesn't say    | 1:15   | 14:20      |
| 13 | The general lesson            | 1:00   | 15:20      |
| 14 | Where it earns its keep       | 0:45   | 16:05      |
| 15 | Why R, specifically           | 0:30   | 16:35      |
| 16 | Honest limitations            | 0:30   | 17:05      |
| 17 | Extensions                    | 0:20   | 17:25      |
| 18 | Takeaways                     | 0:45   | 18:10      |
| 19 | Thank you / contact           | 0:20   | 18:30      |

If running long, the safe cuts are: slide 16 (limitations) and slide 17
(extensions). They can be folded into Q&A.

## Speaker notes — key beats

**Slide 2 (The race).** Let the 0.01 s sit for two beats. Mention that
the IOC genuinely changed the timing rules after this race. Finnish
audience members will already know; international audience members
need this context.

**Slide 3.** This is the pivot slide. Make it explicit: *"I am not here
to tell you whether the beard cost Mieto the gold. I am here to ask
whether it could have."*

**Slide 7 (physics).** If anyone in the audience raises their hand to
say "you're missing rolling resistance, snow friction, etc.", agree
immediately. The model is intentionally minimal. The point is
forward simulation, not skiing physics.

**Slide 8 (anchoring).** The Jensen's-inequality point is the most
technically interesting one for a useR audience. Don't skip it.

**Slide 11 (the result).** This is the money slide. The whole talk
exists to land this image. Pause. Read the subtitle out loud.

Key numbers from the simulation (verified with seed 1980, N = 100,000):

- Median Δt ≈ 0.031 s (about three times the actual margin)
- P(Δt ≥ 0.01 s) ≈ 0.95
- P(Δt ≥ 0.05 s) ≈ 0.24
- The 5th-percentile of the posterior is ≈ 0.010 s.

That last point is worth saying out loud: *the actual margin sits
almost exactly at the 5th percentile of the simulated distribution.*
It is on the low end of what the model expects — but firmly inside it.

**Slide 13 (general lesson).** This is where the talk earns its place
at useR — by being about R and Bayesian thinking, not about skiing.

**Likely Q&A:**
- *Why uniform priors?* — Honesty about ignorance. Beta would be
  defensible too. Worth showing sensitivity in a follow-up.
- *Why not Stan?* — Forward simulation does not need inference.
  The likelihood is the simulator.
- *Could you do this for Wassberg too?* — Yes, and the difference
  distribution is the real counterfactual. Slide 17.
- *Wind?* — Dominates the beard effect. This is exactly why the
  conclusion is conditional, not historical.

## License

CC BY 4.0. Use, remix, present.
