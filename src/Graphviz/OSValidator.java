/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Graphviz;

/**
 *
 * @author Administrator
 */
public class OSValidator {

	private static String OS = System.getProperty("os.name").toLowerCase();
	
//	public static void main(String[] args) {
//		
//		System.out.println(OS);
//		
//		if (isWindows()) {
//			System.out.println("This is Windows");
//		} else if (isMac()) {
//			System.out.println("This is Mac");
//		} else if (isUnix()) {
//			System.out.println("This is Unix or Linux");
//		} else if (isSolaris()) {
//			System.out.println("This is Solaris");
//		} else {
//			System.out.println("Your OS is not support!!");
//		}
//	}

	public static boolean isWindows() {

		return (OS.contains("win"));

	}

	public static boolean isMac() {

		return (OS.contains("mac"));

	}

	public static boolean isUnix() {

		return (OS.contains("nix") || OS.indexOf("nux") >= 0 || OS.indexOf("aix") > 0 );
		
	}

	public static boolean isSolaris() {

		return (OS.contains("sunos"));

	}

}