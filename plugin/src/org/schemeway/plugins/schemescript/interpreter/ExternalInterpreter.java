/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.interpreter;

import gnu.math.*;
import gnu.kawa.io.*;

import java.io.*;

import kawa.standard.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;
import org.eclipse.debug.core.model.*;
import org.eclipse.debug.ui.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.console.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class ExternalInterpreter implements Interpreter {
    
    private static final String ADVICE = "Consult the Error Log view for more information. ";
    
    public static final String CONFIG_TYPE = SchemeScriptPlugin.PLUGIN_NS + ".interpreter";
    
    private static String PID_FILE = ".scheme.pid";
    
    private IProcess mProcess;
    private ILaunch mLaunch;

    public void start() {
        try {
            if (!isRunning()) {
                ILaunchConfigurationType configType = DebugPlugin.getDefault()
                                                                 .getLaunchManager()
                                                                 .getLaunchConfigurationType(CONFIG_TYPE);
                ILaunchConfigurationWorkingCopy copy = configType.newInstance(null, "Scheme");
                mLaunch = copy.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());
                IProcess[] processes = mLaunch.getProcesses();
                
                if (processes.length > 0) {
                    mProcess = mLaunch.getProcesses()[0];
                }
                else {
                    Shell sh = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                    MessageDialog.openError(sh, "Unable to start interpreter!", ADVICE);
                }
            }
        }
        catch (CoreException exception) {
            SchemeScriptPlugin.logException("Unable to start interpreter", exception);
            Shell sh = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            MessageDialog.openError(sh, "Unable to start interpreter!", ADVICE);
            mProcess = null;
        }
        if (isRunning())
            showConsole();
    }

    public void restart() {
        if (isRunning()) {
            stop();
        }
        start();
    }

    public void stop() {
        if (isRunning()) {
            try {
                mProcess.terminate();
            }
            catch (DebugException e) {
                SchemeScriptPlugin.logException("Error while stopping interpreter", e);
            }
            mProcess = null;
        }
    }
    
    public void showConsole() {
        if (mProcess != null) {
          IConsole console = DebugUITools.getConsole(mProcess);
          ConsolePlugin.getDefault().getConsoleManager().showConsoleView(console);
          IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
          IViewPart view = page.findView("org.eclipse.ui.console.ConsoleView");
          if (view != null) 
              view.setFocus();
        }
    }
    
    public boolean isRunning() {
        boolean result = false;
        try {
            if (mProcess != null) {
                result = true;
                mProcess.getExitValue();
                result = false;
            }
        }
        catch (DebugException exception) {
        }
        return result;
    }

    public void eval(String code) {
        if (!isRunning()) {
            start();
        }

        if (!isRunning())
            return;

        try {
            mProcess.getStreamsProxy().write(code + "\n");
        }
        catch (IOException e) {
            SchemeScriptPlugin.logException("Interpreter error: unable to evaluate expression", e);
            Shell sh = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            MessageDialog.openError(sh, "Interpreter error!", ADVICE);
        }
    }

    public void load(IFile file) {
        String filename = file.getRawLocation().toString();
        eval("(load \"" + filename + "\")");
    }

    public boolean supportInterruption() {
        return ExternalInterpreterPreferences.getSavesPID();
    }

    public void interrupt() {
        if (supportInterruption()) {
            String pid = getPID();
            try {
                if (pid != null)
                    Runtime.getRuntime().exec("kill -2 " + pid).waitFor();
            }
            catch (IOException exception) {}
            catch (InterruptedException exception) {}
        }
    }
    
    private String getPID() {
        try {
            String filename = ExternalInterpreterPreferences.getWorkingDirectory().getPath() + "/" + getPIDFilename();
            Object value = readPID(filename);
            if (value instanceof IntNum) {
                return ((IntNum)value).toString();
            }
            return null;
        }
        catch (Throwable exception) {
            return null;
        }
    }
    
    private Object readPID(String filename) throws Throwable {
        try {
            final InPort inport = InPort.openFile(filename);
            try  {
                return Integer.parseInt(read_line.apply(inport, "trim").toString());
            }
            finally {
                inport.close();
            }
        }
        catch (UnsupportedEncodingException e) {
        }
        catch (FileNotFoundException e) {
        }
        return null;
    }
    
    public static String getPIDFilename() {
        return PID_FILE;
    }
}