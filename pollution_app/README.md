Directory pollution_app contains an application extending functionality of pollution and pollution_server modules from previous
laboratories (lab2 and lab3). Module pollution_server was reimplemented using OTP gen_server design pattern. Fault-tolerance of
server is assured by OTP supervisor, which was implemented in pollution_app_supervisor module. 
Whole application can be compiled and booted using rebar3 tool, which was also used to create generic parts of modules code.
