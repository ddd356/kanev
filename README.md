kanev
====

Kanev is a caching server of weather from openweathermap.org data build on a Redis database

# redis
Redis server must be running on a standard port.

# environment
Set environment variables:

- **openweathermap_api_key** <br>
API Key to openweathermap.com
- **openweathermap_settings** <br>
Filepath to settings file

# settings
<p> Prepare settings .json file. </p>
Example:

```json
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
```

Here is:
- **port**                -- port, where your server runs
- **locations**           -- list of locations for which your server will cache data.
- **updatePeriod**        -- how often data will be downloaded (minutes)
- **locationDivergense**  -- divergense between location in "history" query and location in cache (km).
- **timeDivergense**      -- divergense between time in "history" query and time in cache (minutes)

# endpoints
There are two endpoints:
- \current_weather\lat\lon
- \history\lon\lat\utc_time

Example:
- `curl "http://127.0.0.1:8081/current_weather/10.1/11.2"`
- `curl "http://127.0.0.1:8081/history/10.1/11.2/01-01-2021T10:00:00Z"`
