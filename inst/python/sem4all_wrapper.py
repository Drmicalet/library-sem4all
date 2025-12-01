import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
import pandas as pd

class SEM4All:
    def __init__(self):
        """Initialize the SEM4All wrapper."""
        try:
            self.r_package = importr('sem4all')
        except Exception as e:
            print(f"Warning: R package 'sem4all' not found. Ensure it is installed in your R environment. Error: {e}")

    def generate_report(self, model_results, output_dir=".", demographics=None, scales=None, questionnaire=None):
        """
        Generate comprehensive report from model results.
        
        Args:
            model_results (dict): Dictionary of model results (from gsca_elastic or similar)
            output_dir (str): Directory to save output files
            demographics (pd.DataFrame, optional): Demographic data
            scales (pd.DataFrame, optional): Scales data
            questionnaire (pd.DataFrame, optional): Questionnaire data
        """
        # Convert pandas DataFrames to R DataFrames
        r_demographics = ro.r('NULL')
        if demographics is not None:
            with localconverter(ro.default_converter + pandas2ri.converter):
                r_demographics = ro.conversion.py2rpy(demographics)
                
        r_scales = ro.r('NULL')
        if scales is not None:
            with localconverter(ro.default_converter + pandas2ri.converter):
                r_scales = ro.conversion.py2rpy(scales)
                
        r_questionnaire = ro.r('NULL')
        if questionnaire is not None:
            with localconverter(ro.default_converter + pandas2ri.converter):
                r_questionnaire = ro.conversion.py2rpy(questionnaire)
        
        # Call R function
        generate_enhanced_report = ro.r['generate_enhanced_report']
        
        # We need to pass model_results. If it's a Python dict of R objects (from gsca wrapper), 
        # we might need to convert it to an R list.
        # Assuming model_results is a dict where keys are model names and values are result objects.
        
        if isinstance(model_results, dict):
            r_model_results = ro.ListVector(model_results)
        else:
            r_model_results = model_results
            
        generate_enhanced_report(
            model_results=r_model_results,
            demographics=r_demographics,
            scales=r_scales,
            questionnaire=r_questionnaire,
            output_dir=output_dir
        )
