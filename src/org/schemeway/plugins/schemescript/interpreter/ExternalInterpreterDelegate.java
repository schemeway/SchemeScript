/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import java.io.*;
import java.util.*;

import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class ExternalInterpreterDelegate implements ILaunchConfigurationDelegate {
    private static final String WORKDIR_VAR = "\\{workdir\\}";
    private static final String PIDFILE_VAR = "\\{pidfile\\}";
    
    private static String[] mEnvironment;

    public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor)
            throws CoreException {

        if (monitor == null) {
            monitor = new NullProgressMonitor();
        }

        try {
            String[] cmdline = getCommandLine();
            if (cmdline == null || cmdline.length == 0)
                error("Scheme interpreter not set in the preferences");

            Process inferiorProcess = DebugPlugin.exec(cmdline, getWorkingDirectory(), getEnvironment());
            Map attributes = new HashMap();
            attributes.put(IProcess.ATTR_PROCESS_TYPE, "scheme");
            DebugPlugin.newProcess(launch, inferiorProcess, getInterpreterName(), attributes);
        }
        catch (CoreException exception) {
            SchemeScriptPlugin.logException("Unable to start interpreter", exception);
        }
    }

    private String[] getCommandLine() throws CoreException {
        String command = InterpreterPreferences.getCommandLine();
        String workdir = InterpreterPreferences.getWorkingDirectory().getPath();
        workdir = workdir.replaceAll("\\\\", "/"); // HACK for Windows
        command = command.replaceAll(WORKDIR_VAR, workdir);
        if (InterpreterPreferences.getSavesPID()) {
            command = command.replaceAll(PIDFILE_VAR, ExternalInterpreter.getPIDFilename());
        }
        return parseCommandLine(command);
    }
    
    private static String[] parseCommandLine(String command) throws CoreException {
        List list = new ArrayList(10);
        
        int len = command.length();
        int index = 0;
        while (index < len) {
            char ch = command.charAt(index);
            if (ch == '\'') {
                index++;
                int closing = command.indexOf('\'', index);
                if (closing == -1) {
                    error("missing closing single-quote in command line");
                }
                if (closing != index)
                    list.add(command.substring(index, closing));
                index = closing + 1;
            }
            else if (ch == '\"') {
                index++;
                int closing = command.indexOf('\"', index);
                if (closing == -1) {
                    error("missing closing double-quote in command line");
                }
                if (closing != index)
                    list.add(command.substring(index, closing));
                index = closing + 1;
            }
            else if (ch == ' ' || ch == '\t') {
                index++;
            }
            else {
                int start = index;
                index++;
                while (index < len && ch != '\'' && ch != '\"' && ch != ' ' && ch != '\t') {
                    index++;
                    if (index < len)
                        ch = command.charAt(index);
                }
                list.add(command.substring(start, index));
            }
        }
        if (list.size() == 0)
            return null;
        
        return (String[])list.toArray(new String[list.size()]);
    }
    
    private static void error(String message) throws CoreException {
        throw new CoreException(new Status(Status.ERROR,
                                           SchemeScriptPlugin.getDefault().getBundle().getSymbolicName(),
                                           Status.OK,
                                           message,
                                           null));
    }
    
    private synchronized String[] getEnvironment() {
        if (mEnvironment == null) {
            Map systemEnvironment = DebugPlugin.getDefault().getLaunchManager().getNativeEnvironment();
            mEnvironment = new String[systemEnvironment.size() + 1];
            int index = 0;
            Iterator iterator = systemEnvironment.keySet().iterator();
            while (iterator.hasNext()) {
                String name = (String)iterator.next();
                String value = (String)systemEnvironment.get(name);
                mEnvironment[index++] = name + "=" + value;
            }
            mEnvironment[index] = "ECLIPSE=true";
        }
        return mEnvironment;
    }

    private String getInterpreterName() {
        return InterpreterPreferences.getInterpreterName();
    }

    private File getWorkingDirectory() {
        return InterpreterPreferences.getWorkingDirectory();
    }
}