# BUGS:

# Important:
[] Import csv (web) only works for ~2000 lines. e.g. table species.
[] Export as csv (web) do not work in e.g. table Schema.
[X] importDataset is not importing observations with old dates. FIXED!
[X] importDataset is not importing the contributors!  - YES, but only with ORCID.
[X] validateDataset time out in Osorio Canadas or Bartomeus (large list of species) -> now, up to 200 checks (but see below). Works well so far.
[] Removing datasets do not remove its observations.
[] Metadata from advanced search not working e.g. https://traitbase.info/metadata?key=species&value=Bombus%20terrestris 
from simple search it works: https://traitbase.info/metadata?speciesId=5b04145b2265c5000fab2abf
[] When data is downloaded, it lists all contributors of the table per row (only in simple search)
[] When data is downloaded, it lists only the first contributor of the specimen per row (only in advanced search)
[] When downloading data, locationDescription are merged and description not displayed.
[] long shifted to the left (e.g. Bombus terrestris) - checked against Google maps
[] Country and contributor Search not working properly.
[] Some datasets behave wierd and do not import. e.g. Kremen.

# Very Minor or enhancements
[] `UrlPhoto` In table species: this field can be depreciated.
[] Typo: in https://traitbase.info/app#/import/specie The text should read "import Species", not "Specie" NOT IMPORTANT.
[] in /app/dataset observations can be paginated (not important)
[] make ORCID not compulsory to upload contributor. Warn contributor not updated if some compulsory fields are missing. 
[] Add option to deactivate ITIS automatic check. For > 200 species it fails as ITIS is problematic. So far I succeed always!
[] Not let ITIS guess? NOW Validate sp do warns you, How this works exactly? 
[] Return original and uploaded species always (in data)
[] importDataset warns "'contributor_ORCID' is not recognized and it will be ignored." BUT IT WORKS WELL and imports ORCID correctly.
[] Make n_ and se_ missing are interpreted as 1 and 0 respectivelly.
[] Accept observations without m_?
[] Check what does latitude when not numeric... I think it tries to fix it.
[] Countries can be mapped.
[] How it work the search criteria in /app?

#Package
[] make nacho owner of the R package.
[] A Delete dataset (and its observations) function.

#Web
[] Move search box to upper bar.
[] When Date search has day/month blank, it ignores year. Not important.
[] Metadata can be downloaded along with data (maybe a button)
[] We say: "Use our API to connect directly with you preferred programming tool. See details on the API implementation here." Can we refer people to the API, which is the link? I found this: "API REST published over /api on port 5000. Swagger documentation for /api available on /api/documentation"
