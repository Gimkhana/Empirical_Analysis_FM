## Game Theory

Decoding hidden information, coordination, and deception in Loup Garou & Blackjack through mathematical game theory.

Two projects, one theme: Decision under uncertainty.

---

### 1. Loup Garou: Optimal strategy in incomplete information games
Werewolf game modeled as a Bayesian game with asymmetric information. Werewolves know each other; villagers don't. We derived the winning probability formula and proved that optimally-playing werewolves win 41-43% of games despite 3:10 numerical disadvantage.

Key findings:

- Perfect Bayesian Equilibrium exists using "random strategy+" (coordinated modulo voting disguised as randomness)
- Reaching parity (equal werewolves and villagers) = wolf victory with "all-in" coordination
- Seer role shifts balance: 27% wolf win (with Seer) vs. 43% (without)
- Odd player counts favor villagers (more voting rounds); even counts favor wolves

Method: Monte Carlo simulation tested three voter models (random, information-responsive, coordinated). We analyzed 10,000 game outcomes and computed confidence intervals. Critical insight: Information asymmetry beats numerical advantage. Coordination beats deduction (when wolves move together, villagers can't detect the pattern).

Application: Canal+ show analysis (Season 1) showed werewolves exploited quest sabotage mechanics and psychology, converting theoretical 43% into actual victory.

Skill: Python, NumPy, Monte Carlo | Output: Win rate probabilities, sensitivity tables, strategic recommendations

---

### 2. Blackjack Through Game Theory
Blackjack reformulated as a sequential game with imperfect information. Player moves first with only dealer's upcard visible; dealer follows fixed rule (hit <17, stand ≥17). Unlike poker, dealer strategy is not optimized and creating exploitable structure.

Key findings:

- Nash Equilibrium is pure strategy: Stand on 17+, Hit on ≤16

- Optimal "basic strategy" reduces house edge to 0.17%-0.43% (vs. 2-4% for casual players)

- Card counting reverses edge: High count = +1% to +2% player advantage

- Casinos' reshuffle policy at 75% penetration is game-theoretically rational response to counting threat

Why it matters: Blackjack is the only casino game where math proves strategy reduces house edge below 0.5%. Every deviation from basic strategy increases losses. We computed expected value for all 200+ hand/upcard combinations, proving basic strategy optimality.

Variance insight: Short-term outcomes are random despite negative expected value. After 20 hands, outcome could be ±$55. Only after 1000+ hands does the -0.43% EV manifest reliably. This explains why casinos love Blackjack (volume compensates for tiny edge).

Skill: Computational game theory, EV analysis | Output: Basic strategy charts, house edge calculations, card counting metrics

---

**Key takeaway:** 

Games appear intuitive. Math proves otherwise. In Loup Garou, the outnumbered faction wins often because information asymmetry + coordination beats majority voting. In Blackjack, the player with -0.43% edge can still play optimally because expected value doesn't depend on winning the hand (it depends on maximizing long-run aggregate outcomes). Both projects show that rigorous game-theoretic analysis flips conventional wisdom.
