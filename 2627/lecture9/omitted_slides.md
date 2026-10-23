
## General Linear Model (GLM)

<!-- TODO -->
<!-- Follow standard BKO outline -->
<!-- Learning goals summary slide -->
<!-- Have this slide come back each time -->

## The GLM


A family of models to analyze the relationship between one outcome and one or more predictors

- Last week we talked about bivariate linear regression
    * Describes a linear relationship between a continuous outcome variable and a continuous predictor
    * Intercept and slope
    * Assumptions
    * Significance testing

## The GLM

```{dot}
strict digraph GLM {
    graph [
        rankdir=TB,
        bgcolor="transparent",
        nodesep=0.35,
        ranksep=0.85,
        ordering=out
    ];

    node [
        shape=box,
        style="rounded,filled",
        fillcolor="#f1f5f9",
        color="#64748b",
        penwidth=1.5,
        fontname="Helvetica",
        fontsize=15,
        margin="0.18,0.12"
    ];

    edge [
        color="#64748b",
        penwidth=1.5,
        arrowsize=0.7,
        fontname="Helvetica",
        fontsize=11
    ];

    GLM [label="General Linear Models"];
    LM  [label="Linear model"];
    LOG [label="Logistic regression"];

    O [label="One-sample\nt-test"];
    S [label="Simple\nlinear\nregression"];
    I [label="Independent-samples\nt-test"];
    A [label="One-way\nANOVA"];
    M [label="Multiple\nregression"];

    GLM -> LM  [label="  Identity\nlink"];
    GLM -> LOG [label="Logit\nlink"];

    // Highlight the GLM -> LM -> ANOVA branch
    // GLM [penwidth=4, color="#2563eb"];
    // LM  [penwidth=4, color="#2563eb"];
    // O   [penwidth=4, color="#2563eb"];

    // GLM -> LM [penwidth=4, color="#2563eb"];
    // LM  -> O  [penwidth=4, color="#2563eb"];

    LM -> O [label="Intercept only"];
    LM -> S [label="One\ncontinuous\npredictor"];
    LM -> I [label="One\nbinary\npredictor"];
    LM -> A [label="One\ncategorical\npredictor\n(3+ groups)"];
    LM -> M [label="Multiple\npredictors"];

// Separate linear and logistic regression
    {
        rank=same;
        LM;
        spacer [shape=point, width=0.1, style=invis, label=""];
        LOG;
    }

    LM -> spacer -> LOG [
        style=invis,
        weight=100
    ];
    // Keep all five linear-model specializations at the same level
    { rank=same; O; S; I; A; M; }
}
```

## Today we continue with the GLM

```{dot}
strict digraph GLM {
    graph [
        rankdir=TB,
        bgcolor="transparent",
        nodesep=0.35,
        ranksep=0.85,
        ordering=out
    ];

    node [
        shape=box,
        style="rounded,filled",
        fillcolor="#f1f5f9",
        color="#64748b",
        penwidth=1.5,
        fontname="Helvetica",
        fontsize=15,
        margin="0.18,0.12"
    ];

    edge [
        color="#64748b",
        penwidth=1.5,
        arrowsize=0.7,
        fontname="Helvetica",
        fontsize=11
    ];

    GLM [label="General Linear Models"];
    LM  [label="Linear model"];
    LOG [label="Logistic regression"];

    O [label="One-sample\nt-test"];
    S [label="Simple\nlinear\nregression"];
    I [label="Independent-samples\nt-test"];
    A [label="One-way\nANOVA"];
    M [label="Multiple\nregression"];

    GLM -> LM  [label="  Identity\nlink"];
    GLM -> LOG [label="Logit\nlink"];

    // Keep all five linear-model specializations at the same level
    { rank=same; O; S; I; A; M; }

    LM -> O [label="Intercept only"];
    LM -> S [label="One\ncontinuous\npredictor"];
    LM -> I [label="One\nbinary\npredictor"];
    LM -> A [label="One\ncategorical\npredictor\n(3+ groups)"];
    LM -> M [label="Multiple\npredictors"];

    // Highlight the GLM -> LM -> ANOVA branch
    GLM [penwidth=4, color="#2563eb"];
    LM  [penwidth=4, color="#2563eb"];
    S   [penwidth=4, color="#2563eb"];

    GLM -> LM [penwidth=4, color="#2563eb"];
    LM  -> S  [penwidth=4, color="#2563eb"];


// Separate linear and logistic regression
    {
        rank=same;
        LM;
        spacer [shape=point, width=0.1, style=invis, label=""];
        LOG;
    }

    LM -> spacer -> LOG [
        style=invis,
        weight=100
    ];
}
```

