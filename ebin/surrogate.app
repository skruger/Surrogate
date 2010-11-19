{application, surrogate,
 [{description, "Fast Infra - Fast Proxy"},
  {vsn, "0.1"},
  {modules, [ surrogate_app, surrogate_sup, proxy_transparent, proxy_transparent_listener,proxy_socks45,proxy_socks5_listener,
  				proxylib,proxysocket_sup,proxy_connect,proxy_pass,header_parse,filter_check]},
  {registered,[surrogate_sup ]},
  {applications, [kernel,stdlib,crypto]},
  {mod, {surrogate_app,[]}},
  {start_phases,[]}
]}.