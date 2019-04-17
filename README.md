# ZJSON

ABAP implementation for JSON conversions 

## Uploading to SAP
* clone the repositiory using abapGit or as Zip file (ex: $json package)
* unzip and run *switch_namespace.js* utility
  ```bash
    $> node switch_namespace.js <dir> "repo_namespace" "user_namespace"
    $> node switch_namespace.js zjson/ "/abc/" "z"
  ```
* use [abapGit](https://docs.abapgit.org/guide-install.html) to import to SAP
  * zip ZJSON folder and upload

## Pcakages
* *ZJSON* - contains JSON converter classes used by the GraphQL server (prerequisite package)
