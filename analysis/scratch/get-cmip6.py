#!/usr/bin/env python3

import intake

url = "https://storage.cloud.google.com/cmip6/pangeo-cmip6.json"
col = intake.open_esm_datastore(url)
