// launcher.cs --- the PATH's front door on Windows.
//
// SPDX-License-Identifier: MIT
//
// install.ps1 puts %LOCALAPPDATA%\Programs\nodecode\bin on the user PATH and
// this program in it as nodecode.exe. It runs the nodecode.exe of the release
// `current' names as its one child, with this process's own arguments as
// they were typed, and ends the way the child ends.
//
// Why a process of its own: a window keeps the PATH it opened with, and the
// PATH used to name each version's own folder, so a window that ran
// `nodecode update' went on running the release it updated from, and a
// window two updates old found no nodecode at all once that folder was
// pruned. This folder never moves, and what it runs is read at every launch.
//
// Why not `current' on the PATH: Windows 11 refuses an elevated process
// passage through a junction a non-elevated one made (RedirectionGuard,
// error 448), and an SSH login to an administrator is elevated. This program
// reads where the junction points -- its reparse data, which is no passage
// through it -- and starts the release's executable by that real path, so the
// organism runs from its own folder exactly as a launch of it there would.
//
// Built by build-windows.ps1 with the C# compiler every Windows 10 and 11
// carries (.NET Framework 4), so the build needs nothing beyond SBCL and Git.
// C# 5: that compiler knows no later language.

using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;

static class Launcher {
  const uint GENERIC_NONE = 0;
  const uint FILE_SHARE_ALL = 7;
  const uint OPEN_EXISTING = 3;
  const uint FILE_FLAG_BACKUP_SEMANTICS = 0x02000000;
  const uint FILE_FLAG_OPEN_REPARSE_POINT = 0x00200000;
  const uint FSCTL_GET_REPARSE_POINT = 0x000900A8;
  const uint IO_REPARSE_TAG_MOUNT_POINT = 0xA0000003;
  const uint IO_REPARSE_TAG_SYMLINK = 0xA000000C;
  const int ERROR_NOT_A_REPARSE_POINT = 4390;
  const uint CREATE_SUSPENDED = 0x00000004;
  const uint INFINITE = 0xFFFFFFFF;
  const int JobObjectExtendedLimitInformation = 9;
  const uint JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = 0x00002000;
  const uint JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK = 0x00001000;

  [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
  struct STARTUPINFO {
    public int cb;
    public string lpReserved, lpDesktop, lpTitle;
    public int dwX, dwY, dwXSize, dwYSize, dwXCountChars, dwYCountChars, dwFillAttribute, dwFlags;
    public short wShowWindow, cbReserved2;
    public IntPtr lpReserved2, hStdInput, hStdOutput, hStdError;
  }

  [StructLayout(LayoutKind.Sequential)]
  struct PROCESS_INFORMATION {
    public IntPtr hProcess, hThread;
    public int dwProcessId, dwThreadId;
  }

  [StructLayout(LayoutKind.Sequential)]
  struct JOBOBJECT_EXTENDED_LIMIT_INFORMATION {
    public long PerProcessUserTimeLimit, PerJobUserTimeLimit;
    public uint LimitFlags;
    public UIntPtr MinimumWorkingSetSize, MaximumWorkingSetSize;
    public uint ActiveProcessLimit;
    public UIntPtr Affinity;
    public uint PriorityClass, SchedulingClass;
    public ulong ReadOperationCount, WriteOperationCount, OtherOperationCount,
                 ReadTransferCount, WriteTransferCount, OtherTransferCount;
    public UIntPtr ProcessMemoryLimit, JobMemoryLimit, PeakProcessMemoryUsed, PeakJobMemoryUsed;
  }

  delegate bool ConsoleHandler(uint kind);

  [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
  static extern IntPtr CreateFileW(string name, uint access, uint share, IntPtr security,
                                   uint disposition, uint flags, IntPtr template);
  [DllImport("kernel32.dll", SetLastError = true)]
  static extern bool DeviceIoControl(IntPtr device, uint code, IntPtr input, int inputSize,
                                     byte[] output, int outputSize, out int returned, IntPtr overlapped);
  [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
  static extern bool CreateProcessW(string application, string commandLine, IntPtr processSecurity,
                                    IntPtr threadSecurity, bool inheritHandles, uint flags,
                                    IntPtr environment, string directory, ref STARTUPINFO startup,
                                    out PROCESS_INFORMATION info);
  [DllImport("kernel32.dll", CharSet = CharSet.Unicode)]
  static extern IntPtr CreateJobObjectW(IntPtr security, string name);
  [DllImport("kernel32.dll")]
  static extern bool SetInformationJobObject(IntPtr job, int kind,
                                             ref JOBOBJECT_EXTENDED_LIMIT_INFORMATION info, int size);
  [DllImport("kernel32.dll")]
  static extern bool AssignProcessToJobObject(IntPtr job, IntPtr process);
  [DllImport("kernel32.dll")]
  static extern uint ResumeThread(IntPtr thread);
  [DllImport("kernel32.dll")]
  static extern uint WaitForSingleObject(IntPtr handle, uint milliseconds);
  [DllImport("kernel32.dll")]
  static extern bool GetExitCodeProcess(IntPtr process, out uint code);
  [DllImport("kernel32.dll")]
  static extern bool CloseHandle(IntPtr handle);
  [DllImport("kernel32.dll")]
  static extern bool SetConsoleCtrlHandler(ConsoleHandler handler, bool add);
  [DllImport("kernel32.dll")]
  static extern void ExitProcess(uint code);

  static readonly IntPtr Invalid = new IntPtr(-1);
  static IntPtr child = IntPtr.Zero;
  // Held in a field: the runtime may not collect a delegate Windows still calls.
  static readonly ConsoleHandler handler = OnConsoleEvent;

  static bool OnConsoleEvent(uint kind) {
    // Ctrl+C and Ctrl+Break reach the child too, which is attached to the same
    // console and decides what they mean; this process only outlives it. On a
    // closing window, a logoff or a shutdown, Windows ends this process when the
    // handler returns, and the job would end the child with it: wait for the
    // child to finish its own shutdown first.
    if (kind >= 2 && child != IntPtr.Zero) WaitForSingleObject(child, INFINITE);
    return true;
  }

  static void Fail(string text) {
    Console.Error.WriteLine("nodecode: " + text);
    ExitProcess(127);
  }

  // The folder a junction or a symbolic link at PATH names, read from its
  // reparse data; PATH itself when it is a plain folder; null when nothing is
  // there.
  static string LinkTarget(string path) {
    IntPtr handle = CreateFileW(path, GENERIC_NONE, FILE_SHARE_ALL, IntPtr.Zero, OPEN_EXISTING,
                                FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, IntPtr.Zero);
    if (handle == Invalid) return null;
    try {
      byte[] data = new byte[16 * 1024];
      int size;
      if (!DeviceIoControl(handle, FSCTL_GET_REPARSE_POINT, IntPtr.Zero, 0, data, data.Length, out size, IntPtr.Zero)) {
        if (Marshal.GetLastWin32Error() == ERROR_NOT_A_REPARSE_POINT) return path;
        throw new Win32Exception();
      }
      uint tag = BitConverter.ToUInt32(data, 0);
      int at;
      if (tag == IO_REPARSE_TAG_MOUNT_POINT) at = 16;
      else if (tag == IO_REPARSE_TAG_SYMLINK) at = 20;
      else throw new IOException(string.Format("it is a reparse point of a kind no link has (0x{0:X8})", tag));
      int offset = BitConverter.ToUInt16(data, 8), length = BitConverter.ToUInt16(data, 10);
      string target = System.Text.Encoding.Unicode.GetString(data, at + offset, length);
      if (target.StartsWith(@"\??\UNC\")) target = @"\\" + target.Substring(8);
      else if (target.StartsWith(@"\??\")) target = target.Substring(4);
      // A relative symbolic link names its target from the link's own folder.
      if (tag == IO_REPARSE_TAG_SYMLINK && (BitConverter.ToUInt32(data, 16) & 1) != 0)
        target = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(path), target));
      return target;
    } finally { CloseHandle(handle); }
  }

  // What follows the program's own name on a command line, read by the rule
  // CommandLineToArgvW reads that name with: quoted, up to the next quote;
  // bare, up to the first space or tab. The rest goes to the child unparsed, so
  // it reads exactly what was typed.
  static string Arguments(string line) {
    int i = 0;
    if (line.Length > 0 && line[0] == '"') {
      i = line.IndexOf('"', 1);
      i = i < 0 ? line.Length : i + 1;
    } else {
      while (i < line.Length && line[i] != ' ' && line[i] != '\t') i++;
    }
    string rest = line.Substring(i);
    return rest.Length > 0 && rest[0] != ' ' && rest[0] != '\t' ? " " + rest : rest;
  }

  static int Main() {
    string self = System.Reflection.Assembly.GetExecutingAssembly().Location;
    string root = Path.GetDirectoryName(Path.GetDirectoryName(Path.GetFullPath(self)));
    string current = Path.Combine(root, "current");
    string release = null;
    // An install moves `current' by removing the junction and making it again:
    // a launch that lands between the two finds nothing for a moment.
    for (int tries = 0; ; tries++) {
      try { release = LinkTarget(current); }
      catch (Exception e) { Fail("cannot read where " + current + " points: " + e.Message); }
      if (release != null || tries >= 20) break;
      Thread.Sleep(100);
    }
    if (release == null)
      Fail("no release is current under " + root + "; install one: irm https://nodecode.ai/install.ps1 | iex");
    string exe = Path.Combine(release, "nodecode.exe");
    if (!File.Exists(exe))
      Fail(current + " names " + release + ", which holds no nodecode.exe; install again: irm https://nodecode.ai/install.ps1 | iex");

    // The child is in a job that ends it if this process is killed, so no
    // organism runs on after the program the shell waited on is gone; what the
    // child starts is its own (silent breakaway), as a direct launch's is.
    IntPtr job = CreateJobObjectW(IntPtr.Zero, null);
    if (job != IntPtr.Zero) {
      JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits = new JOBOBJECT_EXTENDED_LIMIT_INFORMATION();
      limits.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE | JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK;
      SetInformationJobObject(job, JobObjectExtendedLimitInformation, ref limits,
                              Marshal.SizeOf(typeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION)));
    }
    SetConsoleCtrlHandler(handler, true);

    STARTUPINFO startup = new STARTUPINFO();
    startup.cb = Marshal.SizeOf(typeof(STARTUPINFO));
    PROCESS_INFORMATION info;
    // Handles inherited, nothing redirected: the child reads and writes the
    // console or the pipes this process was given, as if typed itself.
    if (!CreateProcessW(exe, "\"" + exe + "\"" + Arguments(Environment.CommandLine), IntPtr.Zero,
                        IntPtr.Zero, true, CREATE_SUSPENDED, IntPtr.Zero, null, ref startup, out info))
      Fail("cannot start " + exe + ": " + new Win32Exception(Marshal.GetLastWin32Error()).Message);
    child = info.hProcess;
    if (job != IntPtr.Zero) AssignProcessToJobObject(job, info.hProcess);
    ResumeThread(info.hThread);
    CloseHandle(info.hThread);
    WaitForSingleObject(info.hProcess, INFINITE);
    uint code;
    if (!GetExitCodeProcess(info.hProcess, out code)) code = 1;
    // ExitProcess, not a return: a code past int's range (an NTSTATUS such as
    // 0xC000013A, a Ctrl+C) comes through whole.
    ExitProcess(code);
    return 0;
  }
}
