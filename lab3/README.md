Module pingpong contains code that simulates traditional ping pong game, by creating two processes that communicate with each other
as if they were playing ping pong.
Module parcel implements finding closest point from one points set to each point from another points set. Implementation enables calculations 
to be run simultaneously on as many processes as user wishes. The best performance is achieved when the number of parallely running 
processes is equal to the number of cores on your device.
Module server incorporates simple database API from module pollution from lab2 into a global variable server design pattern. It creates one process that holds state of a database and applies requests from other processes to change its content or retrieve from it some data.
