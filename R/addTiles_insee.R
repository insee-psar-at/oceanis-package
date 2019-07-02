addTiles_insee <-
function (map, urlTemplate = "",
                            type = NULL, attribution = NULL, layerId = NULL, group = NULL, options = tileOptions())
{
  options$value = type
  options$attribution = attribution
  invokeMethod(map, getMapData(map), "addTiles", urlTemplate,
               layerId, group, options)
}
