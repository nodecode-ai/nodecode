/* launcher.c --- the gateway's front door on macOS.
 *
 * SPDX-License-Identifier: MIT
 *
 * launchd runs this, and this runs the organism: `launcher PROGRAM ARG...'
 * spawns PROGRAM as its one child, hands it every stop signal it receives,
 * and ends the way the child ends.
 *
 * Why a process of its own: macOS asks the operator about a protected
 * folder -- Desktop, Documents, Downloads -- on behalf of the process
 * launchd started, and remembers the answer against that file, its path
 * and its code hash. The organism's own file moves: every release is a new
 * folder, and a gateway replaces itself with a warm launch image under
 * ~/.cache/nodecode/warm/ whose path changes with every build and every
 * cell edit, and which the cache prunes while it runs. A grant made to
 * any of them is lost at the next move (2026-09-23: Desktop allowed in
 * System Settings, `ls ~/Desktop' still refused). This file never moves
 * and is never rewritten -- background.lisp installs it once per
 * generation -- and a child's access is asked on its parent's behalf, so
 * one answer holds across every update.
 */
#include <errno.h>
#include <signal.h>
#include <spawn.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

extern char **environ;

static const int stops[] = {SIGTERM, SIGINT, SIGHUP, SIGQUIT, SIGUSR1, SIGUSR2};
#define STOPS (sizeof stops / sizeof stops[0])

static volatile pid_t child = 0;

static void forward(int sig) {
  if (child > 0) kill(child, sig);
}

int main(int argc, char **argv) {
  sigset_t held, none;
  posix_spawnattr_t attr;
  struct sigaction action;
  int status, err;
  size_t i;
  pid_t pid;

  if (argc < 2) {
    fprintf(stderr, "usage: %s PROGRAM [ARG...]\n", argv[0]);
    return 64;
  }
  /* Held until the child exists: a stop that lands between the spawn and
     the handler is delivered after, not lost. */
  sigemptyset(&held);
  for (i = 0; i < STOPS; i++) sigaddset(&held, stops[i]);
  sigprocmask(SIG_BLOCK, &held, NULL);

  /* The child starts as launchd would have started it: nothing blocked,
     every stop at its default. */
  sigemptyset(&none);
  posix_spawnattr_init(&attr);
  posix_spawnattr_setsigmask(&attr, &none);
  posix_spawnattr_setsigdefault(&attr, &held);
  posix_spawnattr_setflags(&attr, POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF);
  err = posix_spawn(&pid, argv[1], NULL, &attr, argv + 1, environ);
  posix_spawnattr_destroy(&attr);
  if (err != 0) {
    fprintf(stderr, "launcher: %s: %s\n", argv[1], strerror(err));
    return 127;
  }
  child = pid;

  memset(&action, 0, sizeof action);
  action.sa_handler = forward;
  sigemptyset(&action.sa_mask);
  for (i = 0; i < STOPS; i++) sigaction(stops[i], &action, NULL);
  sigprocmask(SIG_UNBLOCK, &held, NULL);

  while (waitpid(pid, &status, 0) < 0)
    if (errno != EINTR) {
      perror("launcher: waitpid");
      return 1;
    }
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  /* Die of the child's signal, so launchd reads the same end. */
  if (WIFSIGNALED(status)) {
    signal(WTERMSIG(status), SIG_DFL);
    raise(WTERMSIG(status));
    return 128 + WTERMSIG(status);
  }
  return 1;
}
