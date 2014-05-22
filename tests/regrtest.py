#!/usr/bin/python
# Copyright (c) 2009 The Regents of the University of California. All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and its documentation for any purpose, provided that the
# above copyright notice and the following two paragraphs appear in
# all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
# IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
# OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
# TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

import time, subprocess, optparse, sys, socket, os
sys.path.append("../")
import rtest as rtest

# solve      = "nanojs".split()

inputFileExt = "ts"

null       = open("/dev/null", "w")
now	       = (time.asctime(time.localtime(time.time()))).replace(" ","_")
logdir     = "../tests/logs"
logfile    = logdir + "/regrtest_results_%s_%s" % (socket.gethostname (), now)
if not os.path.exists(logdir):
  os.makedirs(logdir)
argcomment = "--! run with "

def logged_sys_call(args, out=None, err=None):
  #print "exec: " + " ".join(args)
  return subprocess.call(args, stdout=out, stderr=err)

def solve_quals(solve, file, bare, time, quiet, flags, dargs):
  if quiet: out = null
  else: out = None
  if time: time = ["time"]
  else: time = []
  if dargs: dargs = ["--" + " --".join(dargs.split())]
  else: dargs = []
  hygiene_flags = []
  out = open(file + ".log", "w")
  rv  = logged_sys_call(time + solve.split() + flags + dargs + hygiene_flags + [file], out)
  out.close()
  return rv

def run_script(file,quiet):
  if quiet: out = null
  else: out = None
  return logged_sys_call(file, out)

def getfileargs(file):
  f = open(file)
  l = f.readline()
  f.close()
  if l.startswith(argcomment):
    return l[len(argcomment):].strip().split(" ")
  else:
    return []

class Config (rtest.TestConfig):
  def __init__ (self, solver, dargs, testdirs, logfile, threadcount):
    rtest.TestConfig.__init__ (self, testdirs, logfile, threadcount)
    self.dargs  = dargs
    self.solver = solver

  def run_test (self, file):
    os.environ['LCCFLAGS'] = self.dargs
    if file.endswith("." + inputFileExt):
      fargs = getfileargs(file)
      return solve_quals(self.solver, file, True, False, True, fargs, self.dargs)
    elif file.endswith(".sh"):
      return run_script(file, True)

  def is_test (self, file):
    return file.endswith("." + inputFileExt)

#####################################################################################

parser = optparse.OptionParser()
parser.add_option("-t", "--threads", dest="threadcount", default=1, type=int, help="spawn n threads")
parser.add_option("-o", "--opts", dest="opts", default="", type=str, help="additional arguments to nanojs")
parser.disable_interspersed_args()
options, args = parser.parse_args()

testDir   = "typescript"
#testDir   = "tsc-src/compiler/core/arrayUtilities"

testSign  = [("pos", 0), ("neg", 1)]
#testSign  = [(".", 0)]

#testCategories = ["arrays", "classes", "lists", "loops", "misc", "objects",
#                  "operators", "simple", "unions"]
## not included: ["proto", "typealias"]

testCategories = ["loops"]
#testCategories = ["."]

testdirs = [("/".join([testDir, s, c]), p) for (s, p) in testSign 
                                           for c      in testCategories ]

runner    = rtest.TestRunner (Config ("nanojs liquid", options.opts, testdirs, logfile, options.threadcount))
runner.run ()

# [os.system(("cd %s; ../../cleanup; cd ../" % d)) for (d,_) in testdirs]  
