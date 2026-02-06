# Figure 1: Design-Conditional Elicitation (DCE) Workflow Diagram

**Paper location:** Main Text, Section 3.1 (Figure 2 in the manuscript refers to this workflow)

## Description

This figure illustrates the complete DCE workflow:

1. **Practitioner Input** — Specify design size *J*, target E[K_J], and uncertainty
2. **TSMM Stage 1 (A1)** — Closed-form initialization via negative-binomial proxy → (a₀, b₀)
3. **TSMM Stage 2 (A2)** — Exact finite-*J* Newton refinement → (a*, b*)
4. **Diagnostics** — Report p(K_J) summaries and dominance risk: Pr(w₁ > 0.5), Pr(w₁ > 0.9)
5. **Decision** — Is dominance risk acceptable?
6. **Dual-Anchor** (optional) — Add weight constraint and report trade-off
7. **Output / Report** — Final α ~ Gamma(a*, b*) with full diagnostics

## How to compile

The figure is created with TikZ in a `standalone` document class. Compile with:

```bash
pdflatex -interaction=nonstopmode Figure_01_framework.tex
```

### Requirements

- A TeX distribution with TikZ (e.g., TeX Live, MiKTeX)
- TikZ libraries: `arrows.meta`, `positioning`, `shapes.geometric`, `fit`, `backgrounds`

## Output

- `Figure_01_framework.pdf` — Vector PDF (also available pre-rendered in `figures/main/Figure_01.pdf`)
