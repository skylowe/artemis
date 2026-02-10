"""
Jupyter Server Proxy configuration for ARTEMIS Shiny App.

Registers the Shiny app as a server-proxy entry so that
JupyterHub can route /proxy/3838/ to the Shiny process.
"""


def setup_shiny():
    return {
        "command": [
            "R",
            "-e",
            (
                "setwd('/home/artemis/project/app'); "
                "source('global.R'); source('ui.R'); source('server.R'); "
                "shiny::shinyApp(ui=ui, server=server, "
                "options=list(host='127.0.0.1', port={port}, launch.browser=FALSE))"
            ),
        ],
        "timeout": 120,
        "launcher_entry": {
            "enabled": True,
            "title": "ARTEMIS",
        },
        "absolute_url": False,
        "new_browser_tab": False,
        "request_timeout": 30,
    }
