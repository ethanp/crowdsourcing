import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Question class holds all the relevant information for each question in the dataset
 */
public class Question
{
	int questionID;
	int goldenJudgement;
	double prior = -1;
	double finalScore = -999;
	Map<String,Integer> workerQs;

	Question(int questionID, int goldenJudgement)
	{
		this.questionID = questionID;
		this.goldenJudgement = goldenJudgement;
		workerQs = new HashMap<String, Integer>();
	}

	public void makePrior()
	{
		// Includes Laplace smoothing
		int sum = 1;
		int count = 2;
		for (int value : workerQs.values())
		{
			sum += value;
			count += 1;
		}
		this.prior = ((double) sum) / count;
	}

	public void addWorkerQ(String workerID, int workerJudgement)
	{
		workerQs.put(workerID, workerJudgement);
	}
}