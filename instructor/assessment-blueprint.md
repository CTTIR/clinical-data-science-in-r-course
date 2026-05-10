# Examination blueprint (no items)

The individual examination in unit 31 has 20 case-based single-best-answer questions in 25 minutes. This file maps
each of the 20 slots to an area, a learning objective and the reasoning a good answer requires. **It contains no
questions, options or answer keys.** Items are written, reviewed and stored in a private, access-controlled
environment chosen by the course team; if no such environment exists, items are not written into any repository.

Distribution: foundations and source criticism 4, testing models 5, complex modelling 4, ethics 2, regulation 2,
data protection 1, information security 2.

| Slot | Area | Workshop | Learning objective | Expected reasoning |
|---:|---|---|---|---|
| 1 | Foundations and source criticism | W1 | Identify the independent unit and denominator in a clinical table | Separate rows, images and people; choose the denominator that matches the question |
| 2 | Foundations and source criticism | W1 | Distinguish descriptive, prognostic and causal questions | Match a question to the comparison it needs |
| 3 | Foundations and source criticism | W2 | Judge whether a cited source supports a claim | Separate existence, topical fit and support for the specific claim |
| 4 | Foundations and source criticism | W2 | Explain how a fluent but false generated sentence arises | Link next-token probabilities to the absence of a truth check |
| 5 | Testing models | W3 | Compute a predictive value from prevalence, sensitivity and specificity | Use natural frequencies; identify the correct denominator |
| 6 | Testing models | W3 | Keep pre-processing and tuning inside training data | Recognise information leaking from test to training |
| 7 | Testing models | W3 | Distinguish discrimination from calibration | Predict the consequence of miscalibration for a threshold |
| 8 | Testing models | W4 | Recognise group leakage in repeated measurements | Choose the split that matches the target claim (new individuals) |
| 9 | Testing models | W4 | Interpret method agreement at the correct level | Aggregate repeated measurements per person; compare limits with a pre-specified tolerance |
| 10 | Complex modelling | W5 | Recognise selection on a collider | Explain an association that appears only in a selected subgroup |
| 11 | Complex modelling | W5 | Interpret trajectories with informative dropout | Identify the dropout mechanism and the appropriate sensitivity analysis |
| 12 | Complex modelling | W5 | Judge whether clusters reflect subtypes or a gradient | Use stability under scaling and resampling as evidence |
| 13 | Complex modelling | W6 | Evaluate a forecast or simulation correctly | Temporal ordering of evaluation windows, or common random numbers and spread over replications |
| 14 | Ethics | W3/W8 | Identify who carries the errors of a threshold | Use subgroup errors with denominators; weigh benefit and harm |
| 15 | Ethics | W5/W8 | Weigh a further animal experiment using the 3Rs | Consider replacement, reduction, refinement and a criterion against the experiment |
| 16 | Regulation | W7 | Derive the regulatory question from the intended purpose | Intended purpose, not technology, decides; Rule 11 for software |
| 17 | Regulation | W7 | Place AI Act obligations in time and by role | Distinguish entry into force, date of application, provider and deployer |
| 18 | Data protection | W7 | Separate the decisions under Articles 33 and 34 GDPR | Risk versus high risk; when the controller became aware |
| 19 | Information security | W7 | Choose a control with an observable test for integrity | Verify the running model version against approved versions |
| 20 | Information security | W2/W7 | Recognise an indirect prompt injection and its control | Enforce permissions outside the assistant; least privilege |

The public [self-test](../exercises/self-test.qmd) contains newly developed practice cases for the same areas. Items
for the examination must be new and must not reuse the self-test cases.
