himpy: multithreaded snmp poller for riemann
============================================

himpy polls common SNMP mibs (bundled in *recipes*),
applies local thresholds and forwards results to
[riemann](http://riemann.io)

## Available MIB Recipes

* network
* storage
* load
* windows services

## Configuration

```json
{
    "logfile": "/tmp/himpy.log",
    "host": "127.0.0.1",
    "port": "5555",
    "hosts": [
        {
            "host": "127.0.0.1",
            "community": "public",
            "recipes": {
                "network": [],
                "storage": [],
                "load": [],
                "winservices": ["logstash"]
            }
        }
    ],
    "thresholds": [
        {
            "host": "(.*)",
            "service": "^/ percent",
            "warning": "30.00",
            "critical": "50.00"
        },
        {
            "host": "(.*)",
            "service": "load-all",
            "warning": "30.00",
            "critical": "50.00"
        }
    ]
}
```

