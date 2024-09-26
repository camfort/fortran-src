# fortran-src JOSS paper submission

## Building the PDF from `paper.md`
With Docker, in your favourite shell (untested):

```
docker run --rm -it \
    -v $PWD:/data \
    -u $(id -u):$(id -g) \
    openjournals/inara \
    -o pdf,crossref \
    paper.md
```

With podman, in the directory containing `paper.md`, in your favourite shell:

```
podman run --volume .:/data --env JOURNAL=joss docker.io/openjournals/inara -o pdf,crossref ./paper.md
```

(Note that the `joss` in `JOURNAL=joss` must be lowercase, at least on
case-sensitive systems.)

See https://github.com/openjournals/inara .

## Resources
Main guidelines:
[Submitting a paper to JOSS](https://joss.readthedocs.io/en/latest/submitting.html)

### Exemplar submissions
  * Paper: [Lots of direct usage examples]
    (https://joss.theoj.org/papers/10.21105/joss.04053)
    (Kamodo, Python, space weather data exploration)
  * Paper: [Text- and reference-heavy]
    (https://joss.theoj.org/papers/10.21105/joss.04205)
    (swyft, Python, ML estimation technique)
  * Paper: [Short & sweet]
    (https://joss.theoj.org/papers/10.21105/joss.04592)
    (SimSGamE, Python, modelling task scheduling in game engines)
  * Paper: [Long with everything]
    (https://joss.theoj.org/papers/10.21105/joss.04568)
    (CWInPy, Python, inference on signals from pulsar stars)
  * Paper: [Long with everything II]
    (https://joss.theoj.org/papers/10.21105/joss.03742)
    (OpenCMP, Python, various simulation inc. fluid dynamics)
  * Review: [With long reviewer checklist]
    (https://github.com/openjournals/joss-reviews/issues/3392)
