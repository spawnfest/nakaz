-type filename() :: string().

-record(net_details, {ip   :: inet:ip_address(),
                      port :: 70..90}).

-record(srv_conf, {iface     :: #net_details{},
                   hostname  :: string(),
                   header    :: binary(),
                   %% conn_type :: http | ssl
                   conn_type :: atom()
                  }).

-record(log_conf, {log :: filename(),
                   %% severity :: debug | info | error
                   severity :: atom()}).
