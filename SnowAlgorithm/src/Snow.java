import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementation of the Snow Algorithm
 *
 * TODO: this currently only works on binary data-sets, but it's extensible
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
		ArrayList<Map> confusionMatrices = makeConfusionMatrices(data);
		return;
	}

	public static ArrayList<Map> makeConfusionMatrices(ArrayList<String[]> data)
	{
		/*
		   Make all the confusion matrices with the following format:
			Index -> index in workerID array, so it is unique for each worker
			yesGivYes -> Count(votedYes & goldYes)
			noGivYes -> Count(votedNo  & goldYes)
			yesGivNo -> Count(votedYes  & goldNo )
			noGivNo -> Count(votedNo  & goldNo)
 		 */
		// Put all the workers in a list
		ArrayList<String> workerID = new ArrayList<String>();
		// Put all the matrices in a list
		ArrayList<Map> workerMatrices = new ArrayList<Map>();
		int addIndex=0, index, currentValue;
		for (String[] dataPoint : data) {
			// worker doesn't already exist, initialize them
			if ((index = workerID.indexOf(dataPoint[1])) == -1)
			{
				workerID.add(dataPoint[1]);
				Map<String, Integer> workerMatrix = new HashMap<String,
					  Integer>();
				workerMatrix.put("Index", addIndex++);
				// if they voted Yes
				if (dataPoint[3].equals("1")){
					// gold voted Yes
					if (dataPoint[4].equals("1")) {
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 0);
						workerMatrix.put("yesGivNo", 0);
						workerMatrix.put("noGivNo", 0);
					} else { // gold voted No
						workerMatrix.put("yesGivYes", 0);
						workerMatrix.put("noGivYes", 0);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 0);
					}
				} else {
					// they voted No, gold voted Yes
					if (dataPoint[4].equals("1")) {
						workerMatrix.put("yesGivYes", 0);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 0);
						workerMatrix.put("noGivNo", 0);
					} else {
						// gold voted No
						workerMatrix.put("yesGivYes", 0);
						workerMatrix.put("noGivYes", 0);
						workerMatrix.put("yesGivNo", 0);
						workerMatrix.put("noGivNo", 1);
					}
				}
				workerMatrices.add(workerMatrix);
			} else { // worker does already exist, update them
				Map<String,Integer> currentMatrix = workerMatrices.get(index);
				if (dataPoint[3].equals("1")){
					// gold voted Yes
					if (dataPoint[4].equals("1")) {
						currentValue = currentMatrix.remove("yesGivYes") + 1;
						currentMatrix.put("yesGivYes", currentValue);
					} else { // gold voted No
						currentValue = currentMatrix.remove("yesGivNo") + 1;
						currentMatrix.put("yesGivNo", currentValue);
					}
				} else {
					// they voted No, gold voted Yes
					if (dataPoint[4].equals("1")) {
						currentValue = currentMatrix.remove("noGivYes") + 1;
						currentMatrix.put("noGivYes", currentValue);
					} else {
						// gold voted No
						currentValue = currentMatrix.remove("noGivNo") + 1;
						currentMatrix.put("noGivNo", currentValue);
					}
				}
			}
		}
		return workerMatrices;
	}

	public static ArrayList<Map> makePriorsAndGold(ArrayList<String[]> data)
	{
		ArrayList<Integer> questionID = new ArrayList<Integer>();
		ArrayList<Integer> qIdCount = new ArrayList<Integer>();
		ArrayList<Integer> qIdYesCount = new ArrayList<Integer>();
		ArrayList<Integer> qIdGoldAns = new ArrayList<Integer>();
		int index, toIncrement;
		for (String[] dataPoint : data)
		{
			// If this Question hasn't been seen, initialize it
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
			// Otherwise, just incorporate its data
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
			// Print out the results
			System.out.printf("QuestionID: %-8d", questionID.get(i));
			System.out.printf("True Answer: %-6d", qIdGoldAns.get(i));
			System.out.printf("Worker Answer: %-5.2f\n", (1.0 * qIdYesCount.get(i) /
				  qIdCount.get(i)));

			// Put the results in an ArrayList of Maps to return
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
