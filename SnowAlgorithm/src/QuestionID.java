/**
 * This didn't end up being necessary
 */
public class QuestionID
{
	String questionID;
	int totalCount;
	int yesCount;
	QuestionID(String passedName, int firstEvaluation)
	{
		questionID = passedName;
		totalCount = 1;
		if (firstEvaluation == 1)
			yesCount = 1;
		else yesCount = 0;
	}

	void foundAnother(int evaluation)
	{
		totalCount++;
		if (evaluation == 1)
			yesCount++;
	}
}
