The SAP's table list reads:

```
14.1.1  Demographics (Safety)
14.2.1  Kaplan-Meier, time to death (FAS)
```

So the TOC is:

```yaml
tflspec_ai: {task: toc, version: 1}
toc:
- {output_id: T-14-1-1, type: table, title: Demographics, population: Safety Population}
- {output_id: F-14-2-1, type: figure, title: "Kaplan-Meier Plot of Time to Death", population: Full Analysis Set}
assumptions:
- "The report numbers follow the SAP's appendix."
```
