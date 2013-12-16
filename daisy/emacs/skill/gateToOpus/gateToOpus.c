/* 
 * This is a C program called gateToOpus
 */


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>

#define SKILLPORT  3
#define WAKEUPFILE "/.gateToOpus.wakeMeUp"
#define TIMEOUTVAL 1000

void sendToSkill( )
{
  int status;
  char buff[100];

  strcpy( buff, "(debugQuit)\n");
  status = write( SKILLPORT, buff, strlen(buff) );

  strcpy( buff, "(load \"~/.dataForOpus\")\n");
  status = write( SKILLPORT, buff, strlen(buff) );
#ifdef DEBUG
  printf( "Sent data to skill\n" );
#endif

  /* Re-register signal handler again: */
  if( signal( SIGUSR1, sendToSkill ) == -1 ) {
    perror( "Signal SIGUSR1 failed to register" );
    exit( -1 );
  }

}

void catchTimer( )
{
  /*
   * See if we've been orphaned, if so then die.
   * Can't easily do full check to see if parent of parent is dead, though
   */
  if( getppid() == 1 ) {
    exit(0);
  }
#ifdef DEBUG
  printf( "Caught timer\n" );
#endif

  /* Re-register signal handler: */
  if( signal( SIGALRM, catchTimer ) == -1 ) {
    perror( "Signal SIGALRM failed to register" );
    exit( -1 );
  }

}

void setTimer()
{
  int which;
  struct itimerval value;

  /*
   * Set up a timer
   */
  value.it_interval.tv_sec = TIMEOUTVAL;
  value.it_interval.tv_usec = 0;
  value.it_value.tv_sec = TIMEOUTVAL;
  value.it_value.tv_usec = 0;

  if( setitimer( ITIMER_REAL, &value, NULL) ) {
    perror( "Failed to set timer: " );
    exit( -5 );
  }
}

void writeWakeUpFile()
{
  char hostname [MAXHOSTNAMELEN+1];
  char filename [200];
  char *home;
  FILE * f;

  if( gethostname(hostname, MAXHOSTNAMELEN) != 0 ) {
    perror( "gethostname() failed" );
    exit( -2 );
  }
#ifdef DEBUG
  printf( "hostname = %s\n", hostname );
#endif
  if( (home = getenv( "HOME" )) == NULL ) {
    fprintf( stderr, "Environment variable 'HOME' not set\n" );
    exit( -3 );
  }

  sprintf( filename, "%s%s", home, WAKEUPFILE );
#ifdef DEBUG
  printf( "filename = %s\n", filename );
#endif
  if( (f = fopen( filename, "w" )) == NULL ) {
    fprintf( stderr, "Couldn't open file %s\n", filename );
    exit( -4 );
  }

  fprintf( f, "REMSH=''; if test %s != `hostname`; ", hostname );
  fprintf( f, "then REMSH='remsh %s'; fi; ", hostname );
  fprintf( f, "$REMSH /bin/kill -%d %d\n", SIGUSR1, getpid() );

  fclose( f );

  if( chmod( filename, S_IRUSR | S_IWUSR | S_IXUSR ) ) {
    printf( stderr, "Error chmod'ing %s", filename );
    perror( "chmod() : " );
    exit( -5 );
  }
}

void main()
{
  /*
   * Catch signals
   */
  if( signal( SIGUSR1, sendToSkill ) == -1 ) {
    perror( "Signal SIGUSR1 failed to register" );
    exit( -1 );
  }

  if( signal( SIGALRM, catchTimer ) == -1 ) {
    perror( "Signal SIGALRM failed to register" );
    exit( -1 );
  }

  /*
   * Write the wake up file and set up timers
   */
  writeWakeUpFile();
  setTimer();

  /*
   * Loop, waiting for signals to arrive
   */

   while( 1 ) {
     pause();
   }
}
