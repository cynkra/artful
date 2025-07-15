# Stat parsin plan

## Overview
1. We need to try and guess the regex which matches the statistic
2. We supply our guesses to the user via blockr.dataeditr package.
users can correct the regex if incorrect.
3. We execute the regex and parse the stats.
If the regex fails, we defauly to returning the stat, as-is.
4. We build an abstraction layer, translating regex to pseudo-patterns, and
vice-versa.

## Major challenges
For step one, we should sign a confidence score to each default statistical pattern.
For this, we can use context to assist in the confidence.

For step four, how can we reliably tranlsate between pseudo patterns to regex?