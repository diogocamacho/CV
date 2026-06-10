# CV System — Diogo M. Camacho

JSON-driven CV system using `vitae` + `awesomecv` in R.
Single source of truth in `resume.json`. Multiple tailored outputs from Rmd templates.

## Structure

```
cv-system/
├── resume.json          # Master record — edit this for all content changes
├── parse_resume.R       # Parser — reads JSON, returns vitae-ready tibbles
├── render_cv.R          # CLI render script
├── DMC_executive.Rmd    # Full executive CV template
├── DMC_az_cvrm.Rmd      # AstraZeneca CVRM tailored template
└── README.md
```

## Workflow

### Update content
Edit `resume.json` only. Never edit the Rmd templates for content changes.

### Render from CLI
```bash
# Full executive CV
Rscript render_cv.R

# Specific template
Rscript render_cv.R DMC_az_cvrm.Rmd

# Render to output folder
Rscript render_cv.R DMC_executive.Rmd output/
```

### Create a new tailored version
1. Add a profile entry to `resume.json` under `profiles`
2. Copy an existing Rmd template, change `PROFILE <- "your_new_profile"`
3. Adjust the summary text in the Rmd header if needed
4. Run `Rscript render_cv.R DMC_yourcompany.Rmd`

### Toggle a role or bullet
In `resume.json`, set `"include": false` on any work entry or highlight.
It disappears from all renders. Set back to `true` to restore it.
**Never delete entries** — the JSON is your archive.

### Add a new role
Add a new object to the `work` array in `resume.json`:
```json
{
  "id": "unique_id",
  "include": true,
  "company": "Company Name",
  "position": "Your Title",
  "location": "City, ST",
  "startDate": "YYYY-MM",
  "endDate": "YYYY-MM",
  "highlights": [
    {
      "include": true,
      "bold": "Headline",
      "text": "Description of what you did."
    }
  ]
}
```

## GitHub workflow
```bash
git pull
# edit resume.json
Rscript render_cv.R DMC_executive.Rmd
git add resume.json DMC_executive.pdf
git commit -m "updated [role/section]"
git push
```

## Requirements
- R >= 4.0
- Packages: `vitae`, `rmarkdown`, `jsonlite`, `dplyr`, `purrr`, `tibble`
- LaTeX distribution (TinyTeX recommended): `tinytex::install_tinytex()`

### Install packages once
```r
install.packages(c("vitae", "rmarkdown", "jsonlite", "dplyr", "purrr", "tibble"))
tinytex::install_tinytex()
```
