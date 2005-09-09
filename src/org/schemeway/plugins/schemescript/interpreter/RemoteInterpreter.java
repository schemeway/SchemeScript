package org.schemeway.plugins.schemescript.interpreter;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.schemeway.plugins.schemescript.SchemeScriptPlugin;

public class RemoteInterpreter implements Interpreter
{
    public static final String CONFIG_TYPE = SchemeScriptPlugin.PLUGIN_NS + ".remoteInterpreter";
    
    public boolean isRunning() {
        return RemoteInterpreterProcess.getInstance().isRunning();
    }

    public void start() {
        if (RemoteInterpreterProcess.getInstance().getLaunch() == null) {
            try {
                ILaunchConfigurationType configType = DebugPlugin.getDefault()
                                                                 .getLaunchManager()
                                                                 .getLaunchConfigurationType(CONFIG_TYPE);
                ILaunchConfigurationWorkingCopy copy = configType.newInstance(null, "");
                copy.launch(ILaunchManager.RUN_MODE, new NullProgressMonitor());
            }
            catch (CoreException exception) {
            }
        }
        showConsole();
    }

    public void stop() {
        RemoteInterpreterProcess process = RemoteInterpreterProcess.getInstance();
        if (process.canTerminate()) {
            try {
                process.terminate();
            } 
            catch (DebugException e) {
            }
        }
    }

    public void restart() {
        if (isRunning()) {
            stop();
        }
        start();
    }

    public boolean supportInterruption() {
        return false;
    }

    public void interrupt() {
    }

    public void eval(String code) {
        if (!isRunning())
            start();
        RemoteInterpreterProcess.getInstance().sendToInterpreter(code + "\n");
    }

    public void load(IFile file) {
        MessageDialog.openInformation(null, "Remote interpreter", "File loading not supported!");
    }

    public void showConsole() {
        IConsole console = DebugUITools.getConsole(RemoteInterpreterProcess.getInstance());
        ConsolePlugin.getDefault().getConsoleManager().showConsoleView(console);
        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        IViewPart view = page.findView("org.eclipse.ui.console.ConsoleView");
        if (view != null)
            view.setFocus();
    }
}
