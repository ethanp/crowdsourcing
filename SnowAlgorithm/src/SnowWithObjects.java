import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementation of the Snow Algorithm
 * TODO: this currently only works on binary data-sets, but it's extensible
 */
public class SnowWithObjects
{
	public static void main(String args[]) throws IOException
	{
		// get the file data formatted for use
		ArrayList<String[]> data = getData
			  ("/Users/Ethan/Dropbox/MLease/all_collected_data/" +
					"rte.standardized.tsv");
		ArrayList<Question> questions = makeQuestions(data);
		for (Question question : questions)
			question.makePrior();
		ArrayList<Map> confusionMatrices = makeConfusionMatrices(data);
		ArrayList<Map> workerWeights = makeWorkerWeights(confusionMatrices);
		ArrayList<Map> posteriorProbabilities = makePosteriorProbabilities
			  (data, questions, workerWeights);
		return;
	}

	public static ArrayList<Map> makePosteriorProbabilities(ArrayList<String[]> data,
	                                                        ArrayList<Question> questions,
	                                                        ArrayList<Map> workerWeights)
	{
		for (Question question : questions)
		{
			break;
		}
		return null;
	}

	public static ArrayList<Map> makeWorkerWeights(ArrayList<Map> confusionMatrices)
	{
		ArrayList<Map> workerWeights = new ArrayList<Map>();
		for (Map<String, Integer> worker : confusionMatrices)
		{
			// retrieve matrix
			int yesGivYes = worker.get("yesGivYes");
			int noGivYes = worker.get("noGivYes");
			int yesGivNo = worker.get("yesGivNo");
			int noGivNo = worker.get("noGivNo");

			// calculate probabilities
			double probWorkerYesGivenYes = ((double) yesGivYes) / (yesGivYes + noGivYes);
			double probWorkerYesGivenNo = ((double) yesGivNo) / (yesGivNo + noGivNo);

			// store probabilities
			Map<String, Double> workerWeight = new HashMap<String, Double>();
			workerWeight.put("probWorkerYesGivenYes", probWorkerYesGivenYes);
			workerWeight.put("probWorkerYesGivenNo", probWorkerYesGivenNo);
			workerWeights.add(workerWeight);
		}
		return workerWeights;
	}

	public static ArrayList<Map> makeConfusionMatrices(ArrayList<String[]> data)
	{
		/*
		   Make all the confusion matrices with the following format:
			Index -> index in workerID array, so it is unique for each worker
			yesGivYes -> Count(votedYes & goldYes)
			noGivYes  -> Count(votedNo  & goldYes)
			yesGivNo  -> Count(votedYes & goldNo )
			noGivNo   -> Count(votedNo  & goldNo )
 		 */
		// Put all the workers in a list
		ArrayList<String> workerID = new ArrayList<String>();
		// Put all the matrices in a list
		ArrayList<Map> workerMatrices = new ArrayList<Map>();
		int addIndex = 0, index, currentValue;
		for (String[] dataPoint : data)
		{
			// worker doesn't already exist, initialize them,
			// Note: Laplace smoothing is built-in here
			if ((index = workerID.indexOf(dataPoint[1])) == -1)
			{
				workerID.add(dataPoint[1]);
				Map<String, Integer> workerMatrix = new HashMap<String,
					  Integer>();
				workerMatrix.put("Index", addIndex++);
				// if they voted Yes
				if (dataPoint[3].equals("1"))
				{
					// gold voted Yes
					if (dataPoint[4].equals("1"))
					{
						workerMatrix.put("yesGivYes", 2);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 1);
					}
					else
					{ // gold voted No
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 2);
						workerMatrix.put("noGivNo", 1);
					}
				}
				else
				{
					// they voted No, gold voted Yes
					if (dataPoint[4].equals("1"))
					{
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 2);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 1);
					}
					else
					{
						// gold voted No
						workerMatrix.put("yesGivYes", 1);
						workerMatrix.put("noGivYes", 1);
						workerMatrix.put("yesGivNo", 1);
						workerMatrix.put("noGivNo", 2);
					}
				}
				workerMatrices.add(workerMatrix);
			}
			else
			{ // worker does already exist, update them
				Map<String, Integer> currentMatrix = workerMatrices.get(index);
				if (dataPoint[3].equals("1"))
				{
					// gold voted Yes
					if (dataPoint[4].equals("1"))
					{
						currentValue = currentMatrix.remove("yesGivYes") + 1;
						currentMatrix.put("yesGivYes", currentValue);
					}
					else
					{ // gold voted No
						currentValue = currentMatrix.remove("yesGivNo") + 1;
						currentMatrix.put("yesGivNo", currentValue);
					}
				}
				else
				{
					// they voted No, gold voted Yes
					if (dataPoint[4].equals("1"))
					{
						currentValue = currentMatrix.remove("noGivYes") + 1;
						currentMatrix.put("noGivYes", currentValue);
					}
					else
					{
						// gold voted No
						currentValue = currentMatrix.remove("noGivNo") + 1;
						currentMatrix.put("noGivNo", currentValue);
					}
				}
			}
		}
		return workerMatrices;
	}

	public static ArrayList<Question> makeQuestions(ArrayList<String[]> data)
	{
		ArrayList<Question> questionObjects = new ArrayList<Question>();
		ArrayList<Integer> seenQuestions = new ArrayList<Integer>();
		for (String[] dataPoint : data)
		{
			int workerJudgement = Integer.parseInt(dataPoint[3]);
			int questionID = Integer.parseInt(dataPoint[2]);
			String workerID = dataPoint[1];

			// If this Question hasn't been seen, initialize a Question for it
			if (!seenQuestions.contains(questionID))
			{
				int goldenJudgement = Integer.parseInt(dataPoint[4]);

				// Make Question object and add it to the list
				Question question = new Question(questionID, goldenJudgement);
				question.addWorkerQ(workerID, workerJudgement);
				questionObjects.add(question);

				seenQuestions.add(questionID);

			} else
			{ // Otherwise add worker & judgement to the appropriate Question object
				for (Question question : questionObjects)
				{
					if (question.getQuestionID() == questionID)
					{
						question.addWorkerQ(workerID, workerJudgement);
						break;
					}
				}
			}
		}

		return questionObjects;
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
