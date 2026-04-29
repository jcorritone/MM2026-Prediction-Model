# March Madness Prediction Model

**Joseph Corritone**  
**Last Updated:** April 2026

*Developed for submission to the Sports Analytics Club at UW-Madison's March Madness Data Challenge 2026.*

## 🏀  Overview
This **March Madness Predictive Model** estimates win probabilities of all possible March Madness matchups (2,278 total). The model attained the top performance in the SAC March Madness Data Challenge, finishing with the lowest Brier Score.

## ⚙️  Workflow
1. **Feature Engineer** team metrics (including offensive efficiency, defensive efficiency, tempo, strength of schedule)
2. **Test Models** using a roller brier evaluation.
3. **Import 2026 bracket CSV to prepare estimations** to better estimate underlying team ability.
4. **Run Monte Carlo simulations** of the entire tournament.
5. **Summarize outcomes** and use to fill out the bracket that maximizes leverage and wins your group

The result: A data-driven view of how the 2026 March Madness tournament will play out, from dominant favorites (Michigan, Duke, Arizona) to under-the-radar teams making deep runs (Iowa, Illinois).

## 📈  Key Findings

*For full detail, see `data/processed/`.*

- **A single-metric logistic regression** emerges as the best model option.
- **Duke and Michigan** appear to be top title contenders.
- **Houston (2 seed) and Illinois (3)** have the clearest paths to a Final 4 for each of their respective seeds.
  
## ⚠️  Limitations 

- The model assumes team health is the same entering and during the tournament as it was throughout the season.
- The model assumes a nuetral site for all tournament games, but does not account for location effects (ex: Wisconsin vs. Florida being played in Milwaukee).
