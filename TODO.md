---
editor_options: 
  markdown: 
    wrap: sentence
---

# modelit To Do list

-   🔥 High Priority: Improve translation management across the package.
    Currently, multiple calls to gettext() and gettextf() from {svMisc} are used.

-   Introduced the first version of tabularise\_\*\*\*() methods for objects such as lm, nls, and glm, initially designed to generate tables using the {flextable} package.
    Future versions will support multiple output formats via {flextable}, {tinytable}, and {gt}.
    To enable this flexibility, a unified internal object structure is being developed to store all relevant metadata (e.g., labels, units, formatting).
    This structure will allow seamless conversion to different table formats using functions like as_flextable(), as_tinytable(), and as_gt().

-   Implement tabularise\_\*\*\*() methods for enriched model objects: nls\_, lm\_, summary.lm\_, anova\_, etc.
    These methods should leverage the metadata (e.g., labels, units) embedded in the enriched objects to produce consistent and informative tables.

-   Complete the examples sections of the tabularise\_\*\*\*() functions for the following object types: lm, summary.lm, nls, summary.nls,...

-   Implement tabularise\_\*\*\*() methods for merMod and summary.merMod objects (e.g., from lme4::glmer()).

-   Extend tabularise() support for anova and aov objects.

    -   Rework the current implementation and determine the appropriate package context ({inferit}, {modelit}, or another).
    -   Also consider integration with car::Anova().
    -   Note: there is currently no proper tabularise_default() method for aov objects.

-   Develop chart() method for lm objects with categorical predictors (e.g., visualizing factor effects).

-   Add support for multiple comparisons.
