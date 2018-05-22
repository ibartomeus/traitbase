#traitbase setup workflow

Here I log all I do for traitbase.info setup

* start with traitbase empty
* go to /app log in as root 
* Create user bartomeus (role admin)
* Create guest user (and role read_only) with psw: guest
* Add species list manually using import species web function. Original file in github::traitbase/processed_data/All_bee_species.csv #NOT DONE FAILING!
* Create bee Schema table manually (this also creates the traits table). Downloaded the created schema to github::traitbase/processed_data/Bee_schema.csv: #NOT DONE AS EXPORT DO NOT WORKS YET.
* Populate the database using github::traitbase/bee_database.R script. This script fetches raw data from the raw_data folder and upload it to traitbase.


