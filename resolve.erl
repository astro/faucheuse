inet_db:add_ns({141,55,192,51}).
inet_res:gethostbyname("www.c3d2.de").                        
inet_res:gethostbyname("spaceboyz.net").                      
inet_db:set_inet6(true).                
inet_res:gethostbyname("spaceboyz.net").
inet_res:gethostbyname("spaceboyz.net").
inet_db:set_inet6(false).               
inet_res:gethostbyname("www.google.com").

resolve(Name) ->
    
