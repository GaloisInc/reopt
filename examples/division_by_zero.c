/* Division-by-zero example for testing processor exceptions and OS
   signals.

   The default Linux behavior is to send sigFPE to the process when it
   generated a division error (#DE) exception.
*/
int main(int argc, char *argv[]) {
  return 1/0;
}
