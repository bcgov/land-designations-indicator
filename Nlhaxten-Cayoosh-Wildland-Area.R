library(bcdata)

ncwa <- bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_PLAN_LEGAL_POLY_SVW") %>%
  filter(STRGC_LAND_RSRCE_PLAN_NAME == 'Lillooet Land and Resource Management Plan',
         LEGAL_FEAT_OBJECTIVE == 'Wildland Area') %>%
  collect()

mapview::mapview(ncwa)

browseURL(ncwa$ENABLING_DOCUMENT_URL[1])
