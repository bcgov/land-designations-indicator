library(bcdata)

ncwa <- bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_PLAN_LEGAL_POLY_SVW") %>%
  filter(STRGC_LAND_RSRCE_PLAN_NAME == 'Lillooet Land and Resource Management Plan',
         LEGAL_FEAT_OBJECTIVE == 'Wildland Area') %>%
  collect()

mapview::mapview(ncwa)

browseURL(ncwa$ENABLING_DOCUMENT_URL[1])

at_fra <- bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_PLAN_LEGAL_POLY_SVW") %>%
  filter(STRGC_LAND_RSRCE_PLAN_NAME == 'Atlin - Taku Strategic Land and Resource Plan',
         LEGAL_FEAT_OBJECTIVE == 'Forest Retention Area') %>%
  collect()

mapview::mapview(at_fra)

browseURL(unique(at_fra$ENABLING_DOCUMENT_URL))

at_rma <- bcdc_query_geodata("WHSE_LAND_USE_PLANNING.RMP_PLAN_LEGAL_POLY_SVW") %>%
  filter(STRGC_LAND_RSRCE_PLAN_NAME == 'Atlin - Taku Strategic Land and Resource Plan',
         LEGAL_FEAT_OBJECTIVE == 'Resource Management Area') %>%
  collect()

mapview::mapview(at_rma)

browseURL(unique(at_rma$ENABLING_DOCUMENT_URL))
