#!/usr/bin/env python

#Author: Seamus Phelan
#Modified: Zsolt Muller (http://muzso.hu/)

#This program runs a custom command/script just before Gnome shuts down. This is
#done the same way as gedit does it (listening for the 'save-yourself' event).
#This is different from placing scipts in /etc/rc*.d/ as the script will be run
#before Gnome exits. If the custom script/command fails with a non-zero return
#code, a popup dialog box will appear offering the chance to cancel logout.
#
#Usage:
#  1 - Place a shell script with the name ".gnome_logout" in your HOME folder
#      and put all your code in it that you want to be executed upon logout
#      from Gnome
#  2 - Run this python script at every Gnome login (add via menu
#      System -> Preferences -> Session)

import sys
import subprocess

import gnome
import gnome.ui
import gtk
import os.path

scriptExecuted = False

class Namespace: pass
ns = Namespace()
ns.dialog = None

def main():
	prog = gnome.init("gnome-logout-daemon", "1.0", gnome.libgnome_module_info_get(), sys.argv, [])
	client = gnome.ui.master_client()
	#set up call back for when 'logout'/'Shutdown' button pressed
	client.connect("die", session_die)
	client.connect("save-yourself", session_save_yourself)
	client.connect("shutdown-cancelled", shutdown_cancelled)

def session_die(*args):
	sys.exit()
	
def session_save_yourself(*args):
	global scriptExecuted
	if scriptExecuted == False:
		scriptExecuted = True
		scriptpath = os.path.expanduser("~/.gnome_logout")
		if os.path.isfile(scriptpath) and os.path.getsize(scriptpath) > 0:
			retcode = subprocess.call(["/bin/sh", scriptpath], shell = False)
			if retcode != 0:
				#command failed  
				show_error_dialog()
	return True

def shutdown_cancelled(*args):
	if ns.dialog != None:
		ns.dialog.destroy()
	return True

def show_error_dialog():
	ns.dialog = gtk.Dialog("There was a problem running your pre-shutdown script",
		None,
		gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
		("There was a problem running your pre-shutdown script - continue logout", gtk.RESPONSE_ACCEPT))
	if ns.test_mode == True:
		response = ns.dialog.run()
		ns.dialog.destroy()
	else:
		#when in shutdown mode gnome will only allow you to open
		#a window using master_client().save_any_dialog()
		#It also adds the 'Cancel logout' button
		gnome.ui.master_client().save_any_dialog(ns.dialog)

#Find out if we are in test mode
if len(sys.argv) >= 2 and sys.argv[1] == "test":
	ns.test_mode = True
else:
	ns.test_mode = False

if ns.test_mode == True:
	main()
	session_save_yourself()
else:
	main()
	gtk.main()
