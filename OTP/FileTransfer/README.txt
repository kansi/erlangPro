The codes work as follows:
    Registry    : stores the names and tracks the file servers.
    Clinet      : reads and writes a file to the file server.
    File Server : stroes data recieved from the client.

- The registry server is started first.
- Then we start a couple of file servers, each of which registers
  with the registry server.
- The registry server elects one of the file server as the master
  server on the first come first server basis.
- Only master server can write data, while all server can handle
  read requests.
  
  
  
    +------+
    |client|
    +------+

Running the code

Note : in order to run the code you should be connected to internet.

1.  Start 3 terminal windows.

2.  a. In terminal 1 go to Client folder.
    b. In terminal 2 go to Server folder.
    c. In terminal 3 go to Registery folder.

3.  a. In terminal 1 start the erlang shell using the following command:
        erl -sname client@localhost

    b. In terminal 2 start the erlang shell using the following command:
        erl -sname serverNode@localhost

    c. In terminal 3 start the erlang shell using the following command:
        erl -sname registry@localhost

4.  Now we are going to connect the shells so that they can talk to each other.

    a. In erlang shell of terminal 1 execute the following commands :
       (client@localhost)1> net_kernel:connect_node(serverNode@localhost).

       (client@localhost)2> net_kernel:connect_node(registry@localhost).

    NOTE : the above commands should return "true" else the shells are not
    connected and the code will not work.

5.  Now we will compile the respective codes

    a. In terminal 1 execute the following :
       (client@localhost)3> c(client).

    b. In terminal 2 execute the following :
       (serverNode@localhost)1> c(fileServer).

    c. In terminal 3 execute the following :
       (registry@localhost)1> c(registryServer).

6.  Now will start the servers.
    NOTE here we will start the registry server first.

    a. In TERMINAL 3 execute the following to start the registry server:
       (registry@localhost)2> registryServer:start_link().

    b. In terminal 2, we will start the file servers using the
    "start_link( serverNameHere )" method of the "fileServer" module we compiled earlier.
    NOTE that the "serverNameHere" can start only with a small character (a-z) and
    NOT with a capital character (A-Z). Following are valid server names :

    myServer1, testServer2, anything3, a3r2Server. So execute the following in
    terminal 2 to start 3 servers named "fileServer1", "fileServer2", "fileServer3".
        
       (serverNode@localhost)2> fileServer:start_link(fileServer1).
       (serverNode@localhost)3> fileServer:start_link(fileServer2).
       (serverNode@localhost)4> fileServer:start_link(fileServer3).

    NOTE : the first server that registers with the registry server will become
    the MASTER SERVER

7.  Now we have all the required servers up and running. So, we will go to the
    terminal 1 to execute the client code

    a.  to write a file to the server use the following function :
        client:clnttcp_create({write, PATH_TO_THE_FILE})
        PATH_TO_FILE : is the relative path to the file from the current dir.

        following is a sample usage

       (client@localhost)4> client:clnttcp_create({write, "../input.txt"})

    b.  To read a file back from the server use the following function :
        client:clnttcp_create({read, NAME_OF_FILE_TO_READ})

        following is a sample usage:
       (client@localhost)5> client:clnttcp_create({read, "input.txt"}).

8.  Now we will test the case where one or more of the server crash.
    NOTE : here we assume that the server will not crash

    a.  In terminal 2 execute the following command to crash the server:
        fileServer:crash(NAME_OF_SERVER)
        for example

        (serverNode@localhost)5> fileServer:crash(server2).

    b.  Now in the termainal 1 we can execute the read command to test the code
        follows:
        (client@localhost)6> client:clnttcp_create({read, "input.txt"}).
