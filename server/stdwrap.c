#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <netinet/in.h>
#include <netdb.h>

int main(int argc, char *argv[])
{ 
    int i, port, sock, sock2, log, limit; 
    struct sockaddr_in server;
    struct hostent *rem;
    struct rlimit lim;

    if (argc < 5) {
        printf("Please give a limit (in bytes), a port number, a file name, and a command\n"); 
        exit(1); 
    }

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0) { 
        perror("socket"); 
        exit(1); 
    }
    if ((sock2 = socket(PF_INET, SOCK_STREAM, 0)) < 0) { 
        perror("socket"); 
        exit(1); 
    }
    if ((rem = gethostbyname("localhost")) == NULL) { 
        perror("gethostbyname"); 
        exit(1); 
    }
    limit=atoi(argv[1]);
    if(limit < 0){
        limit=~0;
    }
    lim.rlim_cur=limit;
    lim.rlim_max=limit;
    if(setrlimit(RLIMIT_AS, &lim)<0){
        perror("setrlimit");
        exit(1);
    }
    port = atoi(argv[2]);
    if(port < 0){
        perror("atoi"); 
        exit(1);
    }
    log = creat(argv[3],S_IRWXU);
    if(log<0){
        perror("creat");
        exit(1);
    }
    server.sin_family = PF_INET;
    bcopy((char*)rem->h_addr, (char*)&server.sin_addr, rem->h_length);
    server.sin_port = htons(port); 
    if (connect(sock,(struct sockaddr*)&server,sizeof(server)) < 0) { 
        perror("connect"); 
        exit(1); 
    }
    sleep(1);
    server.sin_port = htons(port+1); 
    if (connect(sock2,(struct sockaddr*)&server,sizeof(server)) < 0) { 
        perror("connect"); 
        exit(1); 
    }

    dup2(sock,0); 
    dup2(sock2,1);
    dup2(log,2);

    for(i=4; i<argc; i++){
        argv[i-4]=argv[i];
    }
    argv[i-4]=NULL;
    execvp(argv[0], argv);
    perror("execvp");
    return 0; 
}
