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

/**
 * @author Nu Echo Inc.
 */
public class SchemeInterpreterDelegate implements ILaunchConfigurationDelegate {

    public SchemeInterpreterDelegate() {
        super();
    }

    public void launch(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor)
            throws CoreException {

        if (monitor == null) {
            monitor = new NullProgressMonitor();
        }

        try {
            String[] cmdline = getCommandLine();
            if (cmdline == null || cmdline.length == 0)
                throw new CoreException(new Status(Status.ERROR,
                                                   SchemeScriptPlugin.getDefault().getBundle().getSymbolicName(),
                                                   Status.OK,
                                                   "Scheme interpreter not set in the preferences",
                                                   null));

            Process inferiorProcess = DebugPlugin.exec(getCommandLine(), getWorkingDirectory());
            DebugPlugin.newProcess(launch, inferiorProcess, getInterpreterName());
        }
        catch (CoreException exception) {
            SchemeScriptPlugin.logException("Unable to start interpreter", exception);
        }
    }

    private String[] getCommandLine() {
        String command = InterpreterPreferences.getCommandLine();
        StringTokenizer tokenizer = new StringTokenizer(command, " ");
        String[] cmdline = new String[tokenizer.countTokens()];

        for (int i = 0; i < cmdline.length; i++)
            cmdline[i] = tokenizer.nextToken();

        return cmdline;
    }

    private String getInterpreterName() {
        return InterpreterPreferences.getInterpreterName();
    }

    private File getWorkingDirectory() {
        return InterpreterPreferences.getWorkingDirectory();
    }
}