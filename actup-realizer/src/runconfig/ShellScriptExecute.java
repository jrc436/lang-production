package runconfig;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ShellScriptExecute {
	Runtime r;
	public ShellScriptExecute(Runtime r) {
		this.r = r;
	}
	public void execute(String com) throws IOException {
			Process p = r.exec(new String[]{"/bin/sh", "-c", com});
			String s;
			BufferedReader stdin = new BufferedReader(new InputStreamReader(p.getInputStream()));
			BufferedReader stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
			
			s = null;
			while ((s = stderr.readLine()) != null) {
				System.out.println("Error from: "+com);
				System.out.println(s);
			}
			
			s = null;
			while ((s = stdin.readLine()) != null) {
				System.out.println("Output from: "+com);
				System.out.println(s);
			}
		}
	
}
