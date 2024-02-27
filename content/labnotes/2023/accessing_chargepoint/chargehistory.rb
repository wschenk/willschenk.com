#!/usr/bin/env ruby

require 'httparty'

uri = "https://mc.chargepoint.com/map-prod/v2"

headers = {
  'Content-Type':  'application/json',
  }

cookie_hash = HTTParty::CookieHash.new
cookie_hash.add_cookies({
  "auth-session":  "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJ1cm46Y3A6dXNlcjpuYS11czo3YWNlNTIzNy03ZGN" +
                   "lLTQ0MzAtOWU4Yy0wOTZhMGEzZjlkZjYiLCJpYXQiOjE2OTI4MDQ3MDcsIm5iZiI6MTY5Mjg" +
                   "wNDcwNywiZXhwIjoxNjkyODExOTA3LCJpc3MiOiJjaGFyZ2Vwb2ludCIsInJvbGUiOiJkcml" +
                   "2ZXIiLCJub3N1c2VyaWQiOjM5OTM5MywidXNlcmlkIjoidXJuOmNwOnVzZXI6bmEtdXM6N2F" +
                   "jZTUyMzctN2RjZS00NDMwLTllOGMtMDk2YTBhM2Y5ZGY2IiwidXNlcm5hbWUiOiJ3c2NoZW5" +
                   "rQGdtYWlsLmNvbSIsImVtYWlsIjoid3NjaGVua0BnbWFpbC5jb20iLCJyZWdpb24iOiJOQS1" +
                   "VUyIsInRpbWV6b25lIjoiRURUIiwidGltZXpvbmVfb2Zmc2V0IjotMTQ0MDAsInpvbmVfaWQ" +
                   "iOiJBbWVyaWNhL05ld19Zb3JrIiwicHJlZmVycmVkX2lkcF9pZCI6LTEsInRlbmFudF9pZCI" +
                   "6Im5vcyIsImlkcF91c2VyX2lkIjoiIiwianRpIjoiTnpWbVlqRmtaREV0WldSa09DMDBaakZ" +
                   "tTFdFM04yVXROV1l3WkRsbU1ETXlOakEyI1UxNzkwMWQ1I1JOQS1VUyIsImF1ZCI6IkNoYXJ" +
                   "nZVBvaW50In0.2birvce7O_dsbJKBmPHsOov2rTy7Wv3gwxrNzOckgAeSzHFOsokXIufeuFL" +
                   "_-zEm2P0fWCYZybIBSboA29lwMQ",
  "ci_ui_session": "adc0100877dfc4b478ba4faa265158ad%23D61821",
  "locale":        "en-US",
  "country_code":  "US",
  "prefLanguage":  "en",
  "coulomb_sess":  "33bccd354a5c8de968f889d875d5b4e0%23D61821",
  "country_id":    "233",
  "map_latitude":  "38.5",
  "map_longitude": "-95.71",
  "length_unit":   "miles",
  "volume_unit":   "gallons",
  "datadome":      "1NPmBr74KBJuzTugUX8AN33_BIS_foyn~azonNIfQEXJDk_wMd2bOrYLnzGI~bCVzt-YFuyp" +
                   "gLfeJ44W9oqWuYk~9ZrCVjQOgVHmd30H5X8y0K4y2g_kIZLYF5Ifhld_",
  "AWSALB":        "khQvffcFy2tm5xNwqtMiDdBcKtgZLmsf2DJL1cbG2kgkvzvTT6+icysLBRwsDMETlPpAMjFl" +
                   "5KumfPo/mNFK3/H48KLe28Cfj1YJ+DDTJw1vp/sAdvuzd+MJ3mSL",
  "AWSALBCORS":    "khQvffcFy2tm5xNwqtMiDdBcKtgZLmsf2DJL1cbG2kgkvzvTT6+icysLBRwsDMETlPpAMjFl" +
                   "5KumfPo/mNFK3/H48KLe28Cfj1YJ+DDTJw1vp/sAdvuzd+MJ3mSL"
})

headers['Cookie'] = cookie_hash.to_cookie_string

data =
  {
    charging_activity_monthly:
      {
        page_offset: "p_2023_8",
        page_size: 500
      }
  }
page = HTTParty.post uri, body: data.to_json, headers: headers
pp JSON.parse(page.body)
