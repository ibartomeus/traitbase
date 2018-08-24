#traitbase setup workflow

Here I log all I do for traitbase.info setup

* start with traitbase empty
* go to `https://traitbase.info//app` and log in as root 
* Create user ibartomeus (role admin)
* Create guest user (and role read_only) with psw: guest
* Add species table manually using import species web function. Original file in github::traitbase/processed_data/All_bee_species.csv # FAILING partially
* Create bee Schema table manually (this also creates the traits table). Downloaded the created schema to github::traitbase/processed_data/Bee_schema.csv: #NOT DONE AS EXPORT DO NOT WORKS YET.
* Populate the database using github: https://github.com/ibartomeus/traitbase/bee_database.R script. This script fetches raw data from the raw_data folder and upload it to traitbase using package "metadevpro/traitbaser". The script is rather dirty right now.
* Upload manually a few Photos for demo.
* The web content can be acced here: https://github.com/ibartomeus/traitbase_web/issues (private for now.)
* The r package lives here: https://github.com/metadevpro/traitbaser


