# kanev

Kanev is a caching server of weather data build on a Redis database

Set environment variables:

  API Key to openweathermap.com
  openweathermap_api_key

  Filepath to settings file
  openweathermap_settings

Prepare settings .json file.
Example:
{
  "port" : 8081,
  "locations" : [ 
    {
      "lat" : 1.0,
      "lon" : 1.0
    },
    {
      "lat" : 2.0,
      "lon" : 2.0
    }
  ],
  "updatePeriod" : 1,
  "locationDivergense" : 50,
  "timeDivergense" : 5
}
Here is:
  port                -- port, where your server runs
  locations           -- list of locations for which your server will cache data.
  updatePeriod        -- how often data will be downloaded (minutes)
  locationDivergense  -- divergense between location in "history" query and location in cache (km).
  timeDivergense      -- divergense between time in "history" query and time in cache (minutes)

There are two endpoints:
\current_weather\lat\lon
  Example: curl "http://127.0.0.1:8081/current_weather/10/11"
\history\lat\lon\utc_time
  Example: curl "http://127.0.0.1:8081/history/10/11/01-01-2021T10:00:00Z"
