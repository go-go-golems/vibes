import React, { useState, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Textarea } from '@/components/ui/textarea';
import { Checkbox } from '@/components/ui/checkbox';
import { RadioGroup, RadioGroupItem } from '@/components/ui/radio-group';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { trpc } from '@/lib/trpc';
import { toast } from 'sonner';
import { CheckCircle2, XCircle, Loader2 } from 'lucide-react';

interface FieldDefinition {
  name?: string;
  key?: string;
  label?: string;
  title?: string;
  type: string;
  placeholder?: string;
  required?: boolean;
  options?: Array<{ label: string; value: string } | string>;
  correct?: any;
  description?: string;
  rows?: number;
}

interface FormDefinition {
  name?: string;
  title?: string;
  description?: string;
  fields?: FieldDefinition[];
  form?: {
    fields?: FieldDefinition[];
    groups?: Array<{ fields: FieldDefinition[] }>;
  };
}

interface QuizFormProps {
  formId: string;
  definition: FormDefinition;
  documentId: number;
  onSubmit?: (formId: string, responses: Record<string, any>) => void;
  onChange?: (formId: string, responses: Record<string, any>) => void;
  readOnly?: boolean;
  showCorrectAnswers?: boolean;
  existingResponses?: Record<string, any>;
  result?: { score: number; maxScore: number } | null;
  hideSubmitButton?: boolean;
}

export function QuizForm({
  formId,
  definition,
  documentId,
  onSubmit,
  onChange,
  readOnly = false,
  showCorrectAnswers = false,
  existingResponses,
  result: externalResult,
  hideSubmitButton = false,
}: QuizFormProps) {
  const [responses, setResponses] = useState<Record<string, any>>(existingResponses || {});
  const [submitted, setSubmitted] = useState(false);
  const [result, setResult] = useState<{ score: number; maxScore: number } | null>(externalResult || null);

  // Update responses when existingResponses changes
  useEffect(() => {
    if (existingResponses) {
      setResponses(existingResponses);
    }
  }, [existingResponses]);

  // Update result when externalResult changes
  useEffect(() => {
    if (externalResult) {
      setResult(externalResult);
    }
  }, [externalResult]);

  const submitMutation = trpc.quiz.submit.useMutation({
    onSuccess: (data) => {
      setSubmitted(true);
      if (data.score !== null && data.maxScore !== null) {
        setResult({ score: data.score, maxScore: data.maxScore });
        toast.success(`Quiz submitted! Score: ${data.score}/${data.maxScore}`);
      } else {
        toast.success('Quiz submitted successfully!');
      }
      onSubmit?.(formId, responses);
    },
    onError: (error) => {
      toast.error(error.message || 'Failed to submit quiz');
    },
  });

  // Extract fields from various possible structures
  const getFields = (): FieldDefinition[] => {
    if (definition.fields) return definition.fields;
    if (definition.form?.fields) return definition.form.fields;
    if (definition.form?.groups) {
      return definition.form.groups.flatMap(g => g.fields || []);
    }
    return [];
  };

  const fields = getFields();
  const formTitle = definition.name || definition.title;
  const formDescription = definition.description;

  const handleChange = (fieldKey: string, value: any) => {
    const newResponses = { ...responses, [fieldKey]: value };
    setResponses(newResponses);
    onChange?.(formId, newResponses);
  };

  const handleCheckboxChange = (fieldKey: string, optionValue: string, checked: boolean) => {
    const current = responses[fieldKey] || [];
    const newValue = checked 
      ? [...current, optionValue]
      : current.filter((v: string) => v !== optionValue);
    
    const newResponses = { ...responses, [fieldKey]: newValue };
    setResponses(newResponses);
    onChange?.(formId, newResponses);
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (readOnly) return;
    
    submitMutation.mutate({
      documentId,
      formId,
      responses,
    });
  };

  const getFieldKey = (field: FieldDefinition) => field.name || field.key || '';
  const getFieldLabel = (field: FieldDefinition) => field.label || field.title || field.name || field.key || '';

  const normalizeOptions = (options: FieldDefinition['options']): Array<{ label: string; value: string }> => {
    if (!options) return [];
    return options.map(opt => {
      if (typeof opt === 'string') {
        return { label: opt, value: opt };
      }
      return opt;
    });
  };

  const isCorrect = (field: FieldDefinition, value: any): boolean | null => {
    if (field.correct === undefined) return null;
    if (Array.isArray(field.correct)) {
      if (!Array.isArray(value)) return false;
      return field.correct.length === value.length && 
             field.correct.every(c => value.includes(c));
    }
    return value === field.correct;
  };

  const renderField = (field: FieldDefinition, index: number) => {
    const fieldKey = getFieldKey(field);
    const fieldLabel = getFieldLabel(field);
    const value = responses[fieldKey];
    const correct = showCorrectAnswers ? isCorrect(field, value) : null;

    const fieldType = field.type?.toLowerCase() || 'text';

    return (
      <div key={`${fieldKey}-${index}`} className="space-y-2">
        <div className="flex items-center gap-2">
          <Label htmlFor={fieldKey} className="text-sm font-medium">
            {fieldLabel}
            {field.required && <span className="text-red-500 ml-1">*</span>}
          </Label>
          {showCorrectAnswers && correct !== null && (
            correct ? (
              <CheckCircle2 className="h-4 w-4 text-green-500" />
            ) : (
              <XCircle className="h-4 w-4 text-red-500" />
            )
          )}
        </div>
        
        {field.description && (
          <p className="text-sm text-muted-foreground">{field.description}</p>
        )}

        {/* Text Input */}
        {(fieldType === 'text' || fieldType === 'input' || fieldType === 'email' || fieldType === 'number') && (
          <Input
            id={fieldKey}
            type={fieldType === 'input' ? 'text' : fieldType}
            placeholder={field.placeholder}
            value={value || ''}
            onChange={(e) => handleChange(fieldKey, e.target.value)}
            disabled={readOnly || submitted}
            className={correct === false ? 'border-red-500' : correct === true ? 'border-green-500' : ''}
          />
        )}

        {/* Textarea */}
        {fieldType === 'textarea' && (
          <Textarea
            id={fieldKey}
            placeholder={field.placeholder}
            value={value || ''}
            onChange={(e) => handleChange(fieldKey, e.target.value)}
            disabled={readOnly || submitted}
            rows={field.rows || 3}
            className={correct === false ? 'border-red-500' : correct === true ? 'border-green-500' : ''}
          />
        )}

        {/* Select */}
        {(fieldType === 'select' || fieldType === 'dropdown') && (
          <Select
            value={value || ''}
            onValueChange={(v) => handleChange(fieldKey, v)}
            disabled={readOnly || submitted}
          >
            <SelectTrigger className={correct === false ? 'border-red-500' : correct === true ? 'border-green-500' : ''}>
              <SelectValue placeholder={field.placeholder || 'Select an option'} />
            </SelectTrigger>
            <SelectContent>
              {normalizeOptions(field.options).map((opt) => (
                <SelectItem key={opt.value} value={opt.value}>
                  {opt.label}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        )}

        {/* Radio */}
        {(fieldType === 'radio' || fieldType === 'choice') && (
          <RadioGroup
            value={value || ''}
            onValueChange={(v) => handleChange(fieldKey, v)}
            disabled={readOnly || submitted}
            className="space-y-2"
          >
            {normalizeOptions(field.options).map((opt) => (
              <div key={opt.value} className="flex items-center space-x-2">
                <RadioGroupItem value={opt.value} id={`${fieldKey}-${opt.value}`} />
                <Label htmlFor={`${fieldKey}-${opt.value}`} className="font-normal cursor-pointer">
                  {opt.label}
                </Label>
              </div>
            ))}
          </RadioGroup>
        )}

        {/* Checkbox (single) */}
        {fieldType === 'confirm' && (
          <div className="flex items-center space-x-2">
            <Checkbox
              id={fieldKey}
              checked={value || false}
              onCheckedChange={(checked) => handleChange(fieldKey, checked)}
              disabled={readOnly || submitted}
            />
            <Label htmlFor={fieldKey} className="font-normal cursor-pointer">
              {field.placeholder || 'Yes'}
            </Label>
          </div>
        )}

        {/* Checkbox (multiple) */}
        {(fieldType === 'checkbox' || fieldType === 'multi') && (
          <div className="space-y-2">
            {normalizeOptions(field.options).map((opt) => (
              <div key={opt.value} className="flex items-center space-x-2">
                <Checkbox
                  id={`${fieldKey}-${opt.value}`}
                  checked={(value || []).includes(opt.value)}
                  onCheckedChange={(checked) => handleCheckboxChange(fieldKey, opt.value, !!checked)}
                  disabled={readOnly || submitted}
                />
                <Label htmlFor={`${fieldKey}-${opt.value}`} className="font-normal cursor-pointer">
                  {opt.label}
                </Label>
              </div>
            ))}
          </div>
        )}

        {/* Show correct answer if wrong */}
        {showCorrectAnswers && correct === false && field.correct !== undefined && (
          <p className="text-sm text-green-600 dark:text-green-400">
            Correct answer: {Array.isArray(field.correct) ? field.correct.join(', ') : String(field.correct)}
          </p>
        )}
      </div>
    );
  };

  return (
    <Card className="my-6 border-2 border-blue-100 dark:border-blue-900 shadow-sm">
      <CardHeader className="bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-950/50 dark:to-indigo-950/50">
        {formTitle && (
          <CardTitle className="text-lg font-semibold text-blue-900 dark:text-blue-100">
            {formTitle}
          </CardTitle>
        )}
        {formDescription && (
          <CardDescription>{formDescription}</CardDescription>
        )}
      </CardHeader>
      <CardContent className="pt-6">
        <form onSubmit={handleSubmit} className="space-y-6">
          {fields.map((field, index) => renderField(field, index))}
          
          {result && (
            <div className={`p-4 rounded-lg ${
              result.score === result.maxScore 
                ? 'bg-green-50 dark:bg-green-950/50 border border-green-200 dark:border-green-800' 
                : 'bg-blue-50 dark:bg-blue-950/50 border border-blue-200 dark:border-blue-800'
            }`}>
              <p className="font-medium">
                Score: {result.score} / {result.maxScore} ({Math.round((result.score / result.maxScore) * 100)}%)
              </p>
            </div>
          )}
          
          {!hideSubmitButton && !readOnly && !submitted && (
            <Button 
              type="submit" 
              disabled={submitMutation.isPending}
              className="w-full sm:w-auto"
            >
              {submitMutation.isPending ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Submitting...
                </>
              ) : (
                'Submit Quiz'
              )}
            </Button>
          )}
          
          {!hideSubmitButton && submitted && !showCorrectAnswers && (
            <p className="text-sm text-muted-foreground">
              Your response has been recorded.
            </p>
          )}
        </form>
      </CardContent>
    </Card>
  );
}
