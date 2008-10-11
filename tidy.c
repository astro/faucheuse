#include <tidy.h>
#include <buffio.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <arpa/inet.h>



int main(int argc, char **argv )
{
  uint32_t inlen, outlen;
  char* input;
  TidyBuffer output = {0};
  TidyBuffer errbuf = {0};
  int rc = -1;
  Bool ok;

  while(1)
  {
    if (read(STDIN_FILENO, &inlen, sizeof(inlen)) != sizeof(inlen))
      exit(-1);
    inlen = ntohl(inlen);
    //fprintf(stderr, "reading %i bytes\n", inlen);
    input = malloc(inlen + 1);
    if (read(STDIN_FILENO, input, inlen) != inlen)
      exit(-1);
    input[inlen] = '\0';
    //fprintf(stderr, "read %i bytes\n", inlen);

    TidyDoc tdoc = tidyCreate();                     // Initialize "document"

    ok = tidyOptSetBool( tdoc, TidyXhtmlOut, yes );  // Convert to XHTML
    /*if ( ok )
      ok = tidyOptSetBool( tdoc, TidyBodyOnly, yes );*/
    if ( ok )
      rc = tidySetErrorBuffer( tdoc, &errbuf );      // Capture diagnostics
    if ( rc >= 0 )
      rc = tidyParseString( tdoc, input );           // Parse the input
    free(input);

    if ( rc >= 0 )
      rc = tidyCleanAndRepair( tdoc );               // Tidy it up!
    if ( rc >= 0 )
      rc = tidyRunDiagnostics( tdoc );               // Kvetch
    if ( rc > 1 )                                    // If error, force output.
      rc = ( tidyOptSetBool(tdoc, TidyForceOutput, yes) ? rc : -1 );
    if ( rc >= 0 )
      rc = tidySaveBuffer( tdoc, &output );          // Pretty Print

    //fprintf(stderr, "rc = %i\n", rc);
    if ( rc >= 0 )
    {
      outlen = strlen(output.bp) + 1;
      outlen = htonl(outlen);
      write(STDOUT_FILENO, &outlen, sizeof(outlen));
      write(STDOUT_FILENO, "\0", 1);
      write(STDOUT_FILENO, output.bp, strlen(output.bp));
    }
    else
      write(STDOUT_FILENO, "\1\0\0\0\0", 5);

    tidyBufFree( &output );
    tidyBufFree( &errbuf );
    tidyRelease( tdoc );
  }

  return 0;
}
