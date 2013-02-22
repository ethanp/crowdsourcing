import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementation of the Snow Algorithm
 */
public class Snow
{
	public static void main(String args[]) throws IOException
	{
		// get the file data formatted for use
		ArrayList<String[]> data = getData
			  ("/Users/Ethan/Dropbox/MLease/all_collected_data/" +
			  "rte.standardized.tsv");
		ArrayList<Map> priorsAndGold = makePriorsAndGold(data);
	}

	public static ArrayList<Map> makePriorsAndGold(ArrayList<String[]> data)
	{

		// TODO: this currently only works on binary data-sets, but it's extensible
		ArrayList<Integer> questionID = new ArrayList<Integer>();
		ArrayList<Integer> qIdCount = new ArrayList<Integer>();
		ArrayList<Integer> qIdYesCount = new ArrayList<Integer>();
		ArrayList<Integer> qIdGoldAns = new ArrayList<Integer>();
		int index, toIncrement;
		for (String[] dataPoint : data)
		{
			// If this question hasn't been seen, initialize it
			if ((index = questionID.indexOf(Integer.parseInt(dataPoint[2]))) == -1)
			{
				questionID.add(Integer.parseInt(dataPoint[2]));
				qIdCount.add((Integer) 1);
				if (Integer.parseInt(dataPoint[3]) == 1)
					qIdYesCount.add((Integer) 1);
				else qIdYesCount.add((Integer) 0);
				if (Integer.parseInt(dataPoint[4]) == 1)
					qIdGoldAns.add((Integer) 1);
				else qIdGoldAns.add((Integer) 0);
			} else {
			// Otherwise, just grab its data
				toIncrement = qIdCount.remove(index) + 1;
				qIdCount.add(index, toIncrement);
				if (Integer.parseInt(dataPoint[3]) == 1)
				{
					toIncrement = qIdYesCount.remove(index) + 1;
					qIdYesCount.add(index, toIncrement);
				}
			}
		}
		ArrayList<Map> toReturn = new ArrayList<Map>();
		for (int i = 0; i < questionID.size(); i++)
		{
			System.out.printf("QuestionID: %-8d", questionID.get(i));
			System.out.printf("True Answer: %-6d", qIdGoldAns.get(i));
			System.out.printf("Worker Answer: %-5.2f\n", (1.0 * qIdYesCount.get(i) /
				  qIdCount.get(i)));
			Map<String, Integer> question = new HashMap<String, Integer>();
			question.put("questionID", questionID.get(i));
			question.put("goldAnswer", qIdGoldAns.get(i));
			question.put("workerBasedPriorX100",
				  ((100 * qIdYesCount.get(i)) / qIdCount.get(i)));
			toReturn.add(question);
		}
		return toReturn;
	}

	public static ArrayList<String[]> getData(String file) throws IOException
	{
		// Read in the file 'fileName'
		ArrayList<String[]> fileList = new ArrayList<String[]>();
		BufferedReader br = new BufferedReader(new FileReader(file));
		String line;
		while ((line = br.readLine()) != null)
		{
			// break the line into its constituent pieces
			String[] columns = new String[5];
			columns = line.split("\t");
			fileList.add(columns);
		}
		br.close();
		fileList.remove(0);
		return fileList;
	}
}
