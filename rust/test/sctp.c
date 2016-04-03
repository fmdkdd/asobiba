#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/sctp.h>

int main() {
  int sock = socket(PF_INET, SOCK_STREAM, IPPROTO_SCTP);

  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons(3868);

  bind(sock, (struct sockaddr *)&addr, sizeof(addr));

  listen(sock, 1);

  struct sockaddr_in cli_addr;
  int cli_len;

  accept(sock, (struct sockaddr*)&cli_addr, &cli_len);
}
