import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Put the assignment Header here
 */
public class Snow
{
	public static void main(String args[]) throws IOException
	{
		String angerString = getData("Users/Ethan/Dropbox/MLease/all_collected_data/" +
			  "anger.standardized.tsv");
		System.out.println(angerString);
	}
	public static String getData(String fileName) throws IOException
	{
		// Read in the file 'fileName' to one string
		String everything = "";
		BufferedReader br = new BufferedReader(new FileReader(fileName));
		try {
			StringBuilder sb = new StringBuilder();
			String line = br.readLine();

			while (line != null) {
				sb.append(line);
				sb.append("\n");
				line = br.readLine();
			}
			everything = sb.toString();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			br.close();
		}
		return everything;
	}
}
